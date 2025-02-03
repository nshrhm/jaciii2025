# パッケージの読み込み
suppressPackageStartupMessages({
  library(tidyverse)
  library(e1071)
  library(scales)
})

source("R/utils.R")
source("R/translation_config.R")

#' @title クラスタリング結果の散布図作成
#' @param data 入力データ
#' @param membership メンバーシップ行列
#' @param centers クラスターの中心点
#' @param labels クラスターのラベル
#' @param lang 言語コード
#' @return ggplotオブジェクト
create_cluster_plot <- function(data, membership, centers, labels, lang = "ja") {
  tryCatch(
    {
      # データの準備
      analysis_data <- data %>%
        select(starts_with("Q")) %>%
        select(1:2) %>% # Q1とQ2のみを使用
        mutate_all(as.numeric) %>% # 数値型に変換
        scale() # 標準化

      plot_data <- tibble(
        x = analysis_data[, 1],
        y = analysis_data[, 2],
        cluster = factor(max.col(membership),
          levels = 1:length(labels),
          labels = labels
        ),
        membership = apply(membership, 1, max)
      )

      # 中心点の準備
      centers_data <- tibble(
        x = centers[, 1],
        y = centers[, 2],
        cluster = factor(1:length(labels),
          levels = 1:length(labels),
          labels = labels
        )
      )

      # プロット生成
      p <- ggplot(plot_data, aes(x = x, y = y)) +
        geom_point(
          aes(color = cluster, size = membership, alpha = membership)
        ) +
        scale_size_continuous(range = c(2, 5)) +
        scale_alpha_continuous(range = c(0.3, 0.8)) +
        geom_point(
          data = centers_data,
          aes(x = x, y = y),
          color = "black",
          size = 4,
          shape = 8
        ) +
        labs(
          title = get_translation("cluster_title", "plot_labels", lang),
          x = get_translation("Q1", "questions", lang),
          y = get_translation("Q2", "questions", lang),
          color = get_translation("cluster", "cluster_labels", lang),
          size = get_translation("membership", "cluster_labels", lang),
          alpha = get_translation("membership", "cluster_labels", lang)
        ) +
        scale_color_brewer(palette = "Set2") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(size = rel(1.6), hjust = 0.5),
          axis.title = element_text(size = rel(1.6)),
          axis.text = element_text(size = rel(1.6)),
          legend.title = element_text(size = rel(1.6)),
          legend.text = element_text(size = rel(1.1)),
          legend.position = "bottom",
          legend.box = "vertical",
          panel.grid.minor = element_blank()
        )

      return(p)
    },
    error = function(e) {
      warning(sprintf("散布図の作成中にエラー: %s", e$message))
      return(NULL)
    }
  )
}

#' @title Fuzzy c-means クラスタリング実行と可視化
#' @param data アンケートの生データ
#' @param k クラスター数
#' @param m ファジィ化パラメータ
#' @param max_iter 最大反復回数
#' @param lang 言語コード
#' @return クラスタリング結果のリスト
perform_clustering <- function(data, k = 4, m = 4, max_iter = 1000, lang = "ja") {
  tryCatch(
    {
      # データの前処理と正規化
      analysis_data <- data %>%
        select(starts_with("Q")) %>%
        na.omit() %>%
        scale()

      # データの検証
      if (nrow(analysis_data) == 0 || ncol(analysis_data) == 0) {
        stop("前処理後に有効なデータがありません")
      }

      # クラスタリングの実行
      set.seed(123)
      message("\nクラスタリングを開始")

      fuzzy_result <- cmeans(
        analysis_data,
        centers = k,
        m = m,
        iter.max = max_iter,
        dist = "euclidean",
        method = "cmeans",
        verbose = TRUE
      )

      # メンバーシップ行列の検証
      if (is.null(fuzzy_result$membership) || nrow(fuzzy_result$membership) == 0) {
        stop("クラスタリング結果が無効です")
      }

      # クラスターの中心点の平均値を計算
      center_means <- rowMeans(fuzzy_result$centers)

      # クラスターラベルの設定
      label_assignments <- c(
        "high_eval", "mid_eval", "mixed_eval", "low_eval"
      )

      # 中心点の平均値でソート
      sorted_indices <- order(center_means, decreasing = TRUE)
      cluster_labels <- character(k)

      for (i in 1:k) {
        original_index <- which(sorted_indices == i)
        label_key <- if (original_index <= length(label_assignments)) {
          label_assignments[original_index]
        } else {
          sprintf("cluster_%d", i)
        }
        cluster_labels[i] <- get_translation(label_key, "cluster_labels", lang)
      }

      # デバッグ情報の出力
      message("\n=== クラスター分析結果 ===")
      for (i in 1:k) {
        message(sprintf(
          "クラスター %d: 平均値 = %.3f, ラベル = %s",
          i, center_means[i], cluster_labels[i]
        ))
      }

      # クラスタリング結果を返す
      list(
        clusters = fuzzy_result,
        labels = cluster_labels,
        sorted_indices = sorted_indices,
        center_means = center_means
      )
    },
    error = function(e) {
      message("クラスタリング中にエラー: ", e$message)
      return(NULL)
    }
  )
}

#' @title クラスターごとのVASプロット生成
#' @param data 長形式データ
#' @param cluster_info クラスター情報
#' @param plots_dir プロット保存ディレクトリ
#' @param lang 言語コード
#' @param k クラスター数
#' @return プロットのリスト
create_cluster_vas_plots <- function(data, cluster_info, plots_dir, lang = "ja", k = 4) {
  tryCatch(
    {
      # クラスター情報の検証
      if (is.null(cluster_info)) {
        stop("クラスター情報がNULLです")
      }

      cluster_summary <- attr(cluster_info, "cluster_summary")
      if (is.null(cluster_summary)) {
        stop("クラスター概要情報が見つかりません")
      }

      # データの結合と検証
      data_with_clusters <- data %>%
        mutate(ID = as.character(ID)) %>%
        inner_join(
          cluster_info %>%
            mutate(
              ID = as.character(ID),
              cluster = as.factor(cluster)
            ) %>%
            select(ID, cluster),
          by = "ID"
        )

      if (nrow(data_with_clusters) == 0) {
        stop("結合後のデータが空です")
      }

      # プロット生成
      plots <- list()
      ordered_clusters <- cluster_summary$cluster

      for (cluster_label in ordered_clusters) {
        # 現在のクラスターのデータを抽出
        cluster_data <- data_with_clusters %>%
          filter(cluster == cluster_label)

        if (nrow(cluster_data) > 0) {
          message(sprintf(
            "\nクラスター %s のデータを処理中 (%d 行)",
            cluster_label, nrow(cluster_data)
          ))

          # プロット生成
          p <- ggplot(cluster_data, aes(x = question, y = value)) +
            geom_boxplot(width = 0.2, alpha = 0.4, outlier.shape = NA) +
            geom_violin(alpha = 0.4) +
            geom_jitter(alpha = 0.4, width = 0.2, height = 0) +
            labs(
              title = sprintf(
                "%s: %s",
                get_translation("cluster", "cluster_labels", lang),
                cluster_label
              ),
              x = get_translation("x_label", "plot_labels", lang),
              y = get_translation("y_label", "plot_labels", lang)
            ) +
            scale_y_continuous(
              limits = c(0, 1.00),
              breaks = seq(0, 1.00, 0.20)
            ) +
            theme_minimal(base_size = 14) +
            theme(
              plot.title = element_text(size = rel(1.6), hjust = 0.5),
              axis.title = element_text(size = rel(1.6)),
              axis.text = element_text(size = rel(1.6)),
              axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid.minor = element_blank(),
              plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
            )

          plots[[cluster_label]] <- p

          # プロットの保存
          if (!is.null(plots_dir)) {
            safe_name <- gsub(" ", "_", tolower(cluster_label))
            filename_base <- file.path(plots_dir, sprintf("vas_cluster_%s_evaluation", safe_name))

            tryCatch(
              {
                save_results(p, paste0(filename_base, ".pdf"))
                save_results(p, paste0(filename_base, ".svg"))
                message(sprintf("クラスター %s のプロットを保存しました", cluster_label))
              },
              error = function(e) {
                warning(sprintf(
                  "クラスター %s のプロット保存中にエラー: %s",
                  cluster_label,
                  e$message
                ))
              }
            )
          }
        } else {
          warning(sprintf("クラスター %s のデータが空です", cluster_label))
        }
      }

      return(plots)
    },
    error = function(e) {
      warning(sprintf("クラスターVASプロットの作成中にエラー: %s", e$message))
      return(NULL)
    }
  )
}

#' @title メイン実行関数
#' @param input_file 入力ファイルパス
#' @param output_dir 出力ディレクトリ
#' @param lang 言語コード
#' @param verbose デバッグ情報の表示フラグ
#' @param benchmark ベンチマーク実行フラグ
#' @return NULL
main_fuzzy_cmeans <- function(input_file, output_dir, lang = "ja", verbose = FALSE, benchmark = FALSE) {
  tryCatch(
    {
      if (verbose) message("クラスター分析を開始...")

      # 環境設定の初期化
      init_result <- initialize_environment(verbose = TRUE)
      if (is.null(init_result) || !init_result$success) {
        stop("環境設定の初期化に失敗しました")
      }

      # 出力ディレクトリの作成
      plots_dir <- file.path(output_dir, "plots")
      data_dir <- file.path(output_dir, "data")
      dirs <- c(plots_dir, data_dir)
      lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

      # データの読み込みと前処理
      data <- load_and_preprocess_data(input_file, lang)
      if (is.null(data)) {
        stop("データの読み込みに失敗しました")
      }

      # クラスター分析の実行と散布図の生成
      k <- 4
      cluster_results <- perform_clustering(data$raw, k = k, lang = lang)
      if (is.null(cluster_results)) {
        stop("クラスター分析に失敗しました")
      }

      # クラスタリング結果の散布図を生成
      cluster_plot <- create_cluster_plot(
        data = data$raw,
        membership = cluster_results$clusters$membership,
        centers = cluster_results$clusters$centers,
        labels = cluster_results$labels,
        lang = lang
      )

      # 散布図の保存
      if (!is.null(cluster_plot)) {
        save_results(cluster_plot, file.path(plots_dir, "clustering.pdf"))
        save_results(cluster_plot, file.path(plots_dir, "clustering.svg"))
        message("クラスタリング結果の散布図を保存しました")
      } else {
        warning("クラスタリング結果の散布図の生成をスキップしました")
      }

      # クラスター情報の作成
      cluster_info <- tibble(
        ID = data$raw$ID,
        cluster = factor(
          max.col(cluster_results$clusters$membership),
          levels = 1:k,
          labels = cluster_results$labels
        ),
        membership = apply(cluster_results$clusters$membership, 1, max)
      )

      # クラスター概要情報を属性として追加
      attr(cluster_info, "cluster_summary") <- tibble(
        cluster = factor(1:k, levels = 1:k, labels = cluster_results$labels),
        center_mean = cluster_results$center_means,
        sorted_index = cluster_results$sorted_indices
      )

      # クラスター情報の保存
      save_results(cluster_info, file.path(data_dir, "cluster_info.csv"))

      # クラスターごとのVASプロット生成と保存
      create_cluster_vas_plots(data$long, cluster_info, plots_dir, lang, k)

      message("クラスター分析が完了しました")
      return(invisible(TRUE))
    },
    error = function(e) {
      message(sprintf("クラスター分析の実行中にエラー: %s", e$message))
      return(invisible(FALSE))
    }
  )
}
