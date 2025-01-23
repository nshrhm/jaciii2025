# パッケージの読み込み
suppressPackageStartupMessages({
  library(tidyverse)
  library(e1071)
  library(scales)
  # library(ggbeeswarm)
})

source("utils.R")
source("translation_config.R")

#' @title Fuzzy c-means クラスタリング実行と可視化
#' @description ファジィc-meansクラスタリングを実行し、結果を可視化する関数です
#' @param data アンケートの生データ。Q列を含むデータフレームである必要があります
#' @param k クラスター数（デフォルト：4）。データをグループ化する際の目標クラスター数を指定します
#' @param m ファジィ化パラメータ。1に近いほどクリスプなクラスタリング、大きいほどファジィなクラスタリングとなります
#' @param max_iter 最大反復回数。アルゴリズムが収束しない場合の打ち切り回数を指定します
#' @param lang 言語コード。出力メッセージやラベルの言語を指定します（例：'ja'は日本語）
#' @return 以下の要素を含むリスト：
#' \itemize{
#'   \item clusters - クラスタリング結果（メンバーシップ行列、中心点など）
#'   \item plot - クラスタリング結果の散布図
#'   \item data - プロット用の加工済みデータ
#'   \item centers - クラスター中心点の座標
#'   \item labels - クラスターのラベル
#' }
perform_clustering <- function(data, k = 4, m = 4, max_iter = 1000, lang = "ja") {
  tryCatch(
    {
      # データの前処理と正規化
      # Q列のみを選択し、NAを除外
      analysis_data <- data %>%
        select(starts_with("Q")) %>%
        na.omit() %>%
        scale()

      # データの検証
      if (nrow(analysis_data) == 0 || ncol(analysis_data) == 0) {
        stop("前処理後に有効なデータがないため、クラスタリングを実行できません")
      }

      # クラスタリングの実行
      set.seed(123)
      message("\nクラスタリングを開始")
      # クラスタリングの実行
      # centers: クラスター数
      # m: ファジィ化パラメータ（1に近いほどクリスプに、大きいほどファジィに）
      # iter.max: 最大反復回数
      # dist: 距離の計算方法（"euclidean"はユークリッド距離）
      # method: クラスタリング手法（"cmeans"はファジィc-means）
      # verbose: クラスタリング過程の詳細表示
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
      message("\n=== メンバーシップ行列の情報 ===")
      message(
        "メンバーシップ行列のサイズ: ",
        nrow(fuzzy_result$membership), " x ",
        ncol(fuzzy_result$membership)
      )

      if (is.null(fuzzy_result$membership) || nrow(fuzzy_result$membership) == 0) {
        stop("Invalid clustering results")
      }

      # クラスターの中心点情報を分析
      message("\n=== クラスター中心点の分析 ===")
      message("中心点の数: ", nrow(fuzzy_result$centers))
      message("中心点の次元数: ", ncol(fuzzy_result$centers))

      # 各クラスターの中心点の平均値を計算
      center_means <- rowMeans(fuzzy_result$centers)
      message("\n中心点の平均:")
      print(center_means)

      # 中心点の平均値に基づいてクラスターをソート
      sorted_indices <- order(center_means, decreasing = TRUE)

      # クラスターラベルの設定（値に基づいて動的に割り当て）
      label_assignments <- c(
        "high_eval", # 最も高い平均値を持つクラスター
        "mid_eval", # 2番目に高い平均値
        "mixed_eval", # 3番目の平均値
        "low_eval" # 最も低い平均値
      )

      # クラスターラベルの割り当て
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

      # デバッグ情報：各クラスターの特性とラベルの対応を表示
      message("\n=== クラスターラベルの割り当て ===")
      for (i in 1:k) {
        message(sprintf(
          "クラスター %d: 平均 = %.3f, ラベル = %s",
          i,
          center_means[i],
          cluster_labels[i]
        ))
      }

      # プロットデータの準備
      plot_data <- tibble(
        x = analysis_data[, 1],
        y = analysis_data[, 2],
        cluster = factor(max.col(fuzzy_result$membership),
          levels = 1:k,
          labels = cluster_labels
        ),
        membership = apply(fuzzy_result$membership, 1, max)
      )

      # 中心点の準備
      centers <- tibble(
        x = fuzzy_result$centers[, 1],
        y = fuzzy_result$centers[, 2],
        cluster = factor(1:k, levels = 1:k, labels = cluster_labels)
      )

      # プロットの生成
      cluster_plot <- ggplot(plot_data, aes(x = x, y = y)) +
        geom_point(aes(color = cluster, size = membership, alpha = membership)) +
        scale_size_continuous(range = c(2, 5)) +
        scale_alpha_continuous(range = c(0.3, 0.8)) +
        geom_point(
          data = centers,
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
        apply_common_theme()

      message("\n=== クラスタリングが正常に完了しました ===")

      # 結果を返す
      list(
        clusters = fuzzy_result,
        plot = cluster_plot,
        data = plot_data,
        centers = centers,
        labels = cluster_labels
      )
    },
    error = function(e) {
      message("\n=== Error in Clustering ===")
      message("Error details: ", e$message)
      message("Stack trace:")
      print(sys.calls())
      return(NULL)
    }
  )
}

#' @title クラスターごとのVASプロット生成
#' @description クラスタリング結果に基づいて、各クラスターのVAS（Visual Analog Scale）評価を可視化します
#' @param data 長形式データ。question列とvalue列を含む、VAS評価の詳細データ
#' @param cluster_info クラスター情報のデータフレーム。ID、cluster、membershipなどの列を含む
#' @param plots_dir プロット保存ディレクトリ。PDFとSVG形式で保存されます
#' @param lang 言語コード。出力メッセージやラベルの言語を指定します（例：'ja'は日本語）
#' @param k クラスター数（デフォルト：4）。データの分割数を指定します
#' @return 以下の要素を含むggplotオブジェクトのリスト：
#' \itemize{
#'   \item 各クラスターのバイオリンプロット
#'   \item 箱ひげ図
#'   \item ジッタープロットによる個別データ点
#' }
create_cluster_vas_plots <- function(data, cluster_info, plots_dir, lang = "ja", k = 4) {
  tryCatch(
    {
      # データとクラスター情報の結合
      # デバッグ情報の追加
      message("結合前のデータサイズ:")
      message("長形式データの行数:", nrow(data))
      message("クラスター情報の行数:", nrow(cluster_info))

      # IDの型を確認して必要に応じて変換
      data_with_clusters <- data %>%
        mutate(ID = as.character(ID)) %>%
        left_join(
          cluster_info %>%
            mutate(
              ID = as.character(ID),
              cluster = as.factor(cluster)
            ) %>%
            select(ID, cluster),
          by = "ID"
        ) %>%
        filter(!is.na(cluster)) # クラスター情報がないデータを除外

      # 結合後のデータを確認
      message("結合後のデータサイズ:")
      message("結合データの行数:", nrow(data_with_clusters))
      message("一意なクラスター:", paste(unique(data_with_clusters$cluster), collapse = ", "))

      # クラスター情報の表示
      message("\n=== クラスターVASプロット情報 ===")
      cluster_summary <- attr(cluster_info, "cluster_summary")
      if (!is.null(cluster_summary)) {
        message("cluster_infoからクラスター概要を使用します:")
        print(cluster_summary)
      }

      # 各クラスターの現在の状態を表示
      current_summary <- data_with_clusters %>%
        group_by(cluster) %>%
        summarise(
          count = n(),
          .groups = "drop"
        )

      for (i in seq_len(nrow(current_summary))) {
        cluster_label <- current_summary$cluster[i]
        safe_name <- gsub(" ", "_", tolower(cluster_label))
        original_mean <- cluster_summary$center_mean[cluster_summary$cluster == cluster_label]
        message(sprintf(
          "クラスター: %s, サンプル数: %d, 中心点の平均値: %.3f, ファイル名: vas_cluster_%s_evaluation",
          cluster_label,
          current_summary$count[i],
          original_mean,
          safe_name
        ))
      }

      # クラスターごとのプロット生成
      plots <- data_with_clusters %>%
        group_split(cluster) %>%
        map(function(cluster_data) {
          # デバッグ情報
          message("\nクラスターデータを処理中:")
          message("クラスター内の行数: ", nrow(cluster_data))
          cluster_name <- unique(cluster_data$cluster)

          ggplot(cluster_data, aes(x = question, y = value)) +
            geom_violin(alpha = 0.4) +
            geom_boxplot(width = 0.2, alpha = 0.4) +
            geom_jitter(alpha = 0.4, width = 0.2, height = 0) +
            apply_common_theme(rotate_x_labels = TRUE) +
            labs(
              title = sprintf(
                "%s: %s",
                get_translation("cluster", "cluster_labels", lang),
                cluster_name
              ),
              x = get_translation("x_label", "plot_labels", lang),
              y = get_translation("y_label", "plot_labels", lang)
            )
        })

      # クラスター順序に基づいてプロットを保存
      if (!is.null(plots_dir) && !is.null(cluster_summary)) {
        # クラスターの順序を保持
        ordered_clusters <- cluster_summary$cluster

        walk(seq_along(ordered_clusters), function(i) {
          cluster_label <- ordered_clusters[i]
          safe_name <- gsub(" ", "_", tolower(cluster_label))
          filename_base <- file.path(plots_dir, sprintf("vas_cluster_%s_evaluation", safe_name))

          # プロットのインデックスを取得
          plot_index <- which(sapply(data_with_clusters %>% group_split(cluster), function(df) {
            unique(df$cluster) == cluster_label
          }))

          if (length(plot_index) == 1) {
            message(sprintf(
              "クラスター %s のプロットを保存中（平均: %.3f, インデックス: %d）",
              cluster_label,
              cluster_summary$center_mean[i],
              i
            ))

            # PDFとSVGで保存
            save_results(plots[[plot_index]], paste0(filename_base, ".pdf"), type = "pdf")
            save_results(plots[[plot_index]], paste0(filename_base, ".svg"), type = "svg")
          }
        })
      }

      return(plots)
    },
    error = function(e) {
      warning(sprintf("Error in cluster VAS plots creation: %s", e$message))
      return(NULL)
    }
  )
}

#' @title ファジィクラスタリング分析の実行と結果の可視化
#' @description データの読み込みから前処理、クラスタリング、結果の可視化までの一連の処理を実行します
#' @param input_file 入力ファイルパス。分析対象のデータファイルを指定します
#' @param output_dir 出力ディレクトリ。結果ファイルやプロットが保存されます
#' @param lang 言語コード。出力メッセージやラベルの言語を指定します（例：'ja'は日本語）
#' @param verbose デバッグ情報の表示フラグ。TRUEの場合、詳細な処理情報を出力します
#' @param benchmark ベンチマーク実行フラグ。TRUEの場合、処理時間の計測を行います
#' @return NULL。結果は指定されたディレクトリに保存されます：
#' \itemize{
#'   \item クラスタリング結果のCSVファイル
#'   \item クラスタリング結果の散布図（PDF/SVG）
#'   \item クラスターごとのVASプロット（PDF/SVG）
#' }
main_fuzzy_cmeans <- function(input_file, output_dir, lang = "ja", verbose = FALSE, benchmark = FALSE) {
  # クラスター分析
  # 開始時刻を記録
  start_time <- Sys.time()
  if (verbose) message("クラスター分析を開始...")

  # 環境設定の初期化
  # verbose=TRUE: デバッグ情報を表示
  # install_packages=TRUE: 必要なパッケージを自動インストール
  # force_cairo=FALSE: 必要な場合のみCairoを使用
  init_result <- tryCatch(
    {
      initialize_environment(verbose = TRUE)
    },
    error = function(e) {
      message("環境設定の初期化中にエラーが発生しました: ", e$message)
      return(NULL)
    }
  )

  # 初期化処理の終了時刻を記録
  end_time <- Sys.time()

  # 初期化結果の確認とログ出力
  if (!is.null(init_result)) {
    # 処理時間の計算と表示
    processing_time <- difftime(end_time, start_time, units = "secs")
    message(sprintf("初期化処理の所要時間: %.2f秒", processing_time))

    # 初期化結果の表示
    if (init_result$success) {
      message("環境設定の初期化が正常に完了しました")
      message(sprintf("- 設定されたロケール: %s", init_result$locale))
      message(sprintf("- グラフィックデバイス: %s", init_result$graphics_device))
    } else {
      warning("環境設定の初期化で一部エラーが発生しました")
      # エラーメッセージの表示
      cat("詳細メッセージ:\n")
      cat(paste("- ", init_result$messages, collapse = "\n"))
    }
  } else {
    stop("環境設定の初期化に失敗しました")
  }

  if (verbose) message("必要なディレクトリを作成...")
  # 出力ディレクトリ構造を設定
  plots_dir <- file.path(output_dir, "plots")
  data_dir <- file.path(output_dir, "data")
  dirs <- c(plots_dir, data_dir)
  lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

  if (verbose) message("データの読み込みと前処理を開始...")
  data <- load_and_preprocess_data(input_file, lang)

  if (is.null(data)) {
    message("データの読み込みに失敗しました")
    return(NULL)
  }

  # クラスター数の設定
  k <- 4

  # クラスター分析
  cluster_results <- tryCatch(
    {
      perform_clustering(data$raw, k = k, lang = lang)
    },
    error = function(e) {
      warning(sprintf("クラスタリング中にエラーが発生しました: %s", e$message))
      return(NULL)
    }
  )

  # クラスター情報の処理
  cluster_info <- NULL
  if (!is.null(cluster_results) && !is.null(cluster_results$clusters)) {
    cluster_info <- tryCatch(
      {
        # デバッグ情報の追加
        message("\n=== クラスター情報の処理 ===")

        # クラスターのメンバーシップ情報を取得
        membership_matrix <- cluster_results$clusters$membership
        message(
          "メンバーシップ行列のサイズ: ",
          nrow(membership_matrix), " x ", ncol(membership_matrix)
        )

        max_cluster_indices <- max.col(membership_matrix)
        message("最大クラスターインデックスの数: ", length(max_cluster_indices))

        max_membership_values <- apply(membership_matrix, 1, max)
        message("最大メンバーシップ値の数: ", length(max_membership_values))

        # データフレームの作成前の確認
        message("ID数: ", length(data$raw$ID))

        # クラスターの特性分析
        center_means <- rowMeans(cluster_results$clusters$centers)
        sorted_indices <- order(center_means, decreasing = TRUE)

        # クラスターラベルの動的割り当て
        label_assignments <- c(
          "high_eval", # 最も高い平均値を持つクラスター
          "mid_eval", # 2番目に高い平均値
          "mixed_eval", # 3番目の平均値
          "low_eval" # 最も低い平均値
        )

        # クラスターラベルの生成
        cluster_labels <- character(k)

        # デバッグ情報：クラスターラベルと中心点の対応を詳細に表示
        message("\n=== 詳細なクラスター分析 ===")
        for (i in 1:k) {
          # クラスター番号iに対応する、ソート後のインデックスを取得
          original_index <- which(sorted_indices == i)
          # ラベルキーの設定
          label_key <- if (original_index <= length(label_assignments)) {
            label_assignments[original_index]
          } else {
            sprintf("cluster_%d", i)
          }
          # ラベルの翻訳と設定
          translated_label <- get_translation(label_key, "cluster_labels", lang)
          cluster_labels[i] <- translated_label

          # ファイル名用の文字列を生成
          safe_name <- gsub(" ", "_", tolower(translated_label))

          # デバッグ情報の出力
          message(sprintf(
            "中心点の平均値: %.3f, クラスター番号: %d, ラベル: %s, ファイル名: vas_cluster_%s_evaluation",
            center_means[i],
            i,
            translated_label,
            safe_name
          ))
        }

        message("\n=== Cluster Analysis ===")
        for (i in 1:k) {
          message(sprintf(
            "Cluster %d: Mean = %.3f, Label = %s",
            i,
            center_means[i],
            cluster_labels[i]
          ))
        }

        # クラスターラベルをベクトルとして保持
        cluster_labels_vec <- cluster_labels

        # クラスター情報をデータフレームとして保持
        result_df <- tibble(
          ID = data$raw$ID,
          cluster = factor(max_cluster_indices,
            levels = 1:k,
            labels = cluster_labels_vec
          ),
          membership = max_membership_values,
          cluster_mean = center_means[max_cluster_indices] # 各データポイントのクラスター中心点平均値を追加
        )

        # クラスター情報の要約を属性として付加
        cluster_summary <- tibble(
          cluster = factor(1:k, levels = 1:k, labels = cluster_labels_vec),
          center_mean = center_means,
          sorted_index = sorted_indices
        )
        attr(result_df, "cluster_summary") <- cluster_summary

        message(
          "作成されたクラスター情報データフレームのサイズ: ",
          nrow(result_df), " x ", ncol(result_df)
        )
        message("\nクラスター概要:")
        print(cluster_summary)

        result_df
      },
      error = function(e) {
        message("Error in cluster info processing: ", e$message)
        message("Stack trace:")
        print(sys.calls())
        return(NULL)
      }
    )
  }

  # クラスター情報の保存
  if (!is.null(cluster_info)) {
    message("\n=== クラスター結果の保存 ===")

    # cluster_infoの保存
    message("クラスター情報をCSVに保存中...")

    save_results(cluster_info, file.path(data_dir, "cluster_info.csv"))

    # プロットの保存を試行
    message("cluster_results$plotを確認中...")
    if (!is.null(cluster_results$plot)) {
      message("プロットオブジェクトが存在します。保存を試みます...")
      tryCatch(
        {
          save_results(cluster_results$plot, file.path(plots_dir, "clustering.pdf"), type = "pdf")
          save_results(cluster_results$plot, file.path(plots_dir, "clustering.svg"), type = "svg")
        },
        error = function(e) {
          message("プロットの保存中にエラー: ", e$message)
          message("プロットオブジェクトの構造:")
          str(cluster_results$plot)
        }
      )
    } else {
      message("cluster_results$plotがNULLです")
    }
    # クラスターごとのVASプロット生成と保存
    message("\nクラスターVASプロットを作成中...")
    tryCatch(
      {
        cluster_plots <- create_cluster_vas_plots(data$long, cluster_info, plots_dir, lang, k = k)
        if (!is.null(cluster_plots)) {
          message("クラスターVASプロットを保存中...")
          # cluster_summary から順序付けられたラベルを取得
          cluster_summary <- attr(cluster_info, "cluster_summary")
          if (is.null(cluster_summary)) {
            # cluster_summary が無い場合は既存の方法で処理
            cluster_labels <- unique(cluster_info$cluster)
            message("クラスターのプロットを保存中（順序なし）: ", paste(cluster_labels, collapse = ", "))

            walk(seq_along(cluster_plots), function(i) {
              cluster_label <- cluster_labels[i]
              safe_name <- gsub(" ", "_", tolower(cluster_label))
              filename_base <- file.path(plots_dir, sprintf("vas_cluster_%s_evaluation", safe_name))
              message("クラスターのプロットを保存中: ", cluster_label)

              tryCatch(
                {
                  save_results(cluster_plots[[i]], paste0(filename_base, ".pdf"), type = "pdf")
                  save_results(cluster_plots[[i]], paste0(filename_base, ".svg"), type = "svg")
                  message("クラスターのプロットを正常に保存しました: ", cluster_label)
                },
                error = function(e) {
                  message("クラスター ", cluster_label, " のプロット保存中にエラー: ", e$message)
                }
              )
            })
          } else {
            # cluster_summary を使用して順序付けられたプロット保存
            ordered_clusters <- cluster_summary$cluster
            message("クラスターのプロットを保存中（平均値で順序付け）: ", paste(ordered_clusters, collapse = ", "))

            walk(seq_along(ordered_clusters), function(i) {
              cluster_label <- ordered_clusters[i]
              safe_name <- gsub(" ", "_", tolower(cluster_label))
              filename_base <- file.path(plots_dir, sprintf("vas_cluster_%s_evaluation", safe_name))

              # クラスターラベルに対応するプロットのインデックスを取得
              plot_index <- which(sapply(seq_along(plots), function(j) {
                cluster_data <- data_with_clusters %>%
                  group_split(cluster) %>%
                  .[[j]]
                unique(cluster_data$cluster) == cluster_label
              }))

              if (length(plot_index) == 1) {
                message(sprintf(
                  "Saving plot for cluster %s (mean: %.3f, index: %d)",
                  cluster_label,
                  cluster_summary$center_mean[i],
                  i
                ))

                tryCatch(
                  {
                    save_results(cluster_plots[[plot_index]], paste0(filename_base, ".pdf"), type = "pdf")
                    save_results(cluster_plots[[plot_index]], paste0(filename_base, ".svg"), type = "svg")
                    message("クラスターのプロットを正常に保存しました: ", cluster_label)
                  },
                  error = function(e) {
                    message("クラスター ", cluster_label, " のプロット保存中にエラー: ", e$message)
                  }
                )
              }
            })
          }
        } else {
          message("cluster_plotsがNULLです")
        }
      },
      error = function(e) {
        message("クラスターVASプロットの作成中にエラー: ", e$message)
      }
    )
  }
}
