# このスクリプトは、Fuzzy C-means (FCM) クラスタリングを用いて、アンケートデータを分析し、結果を可視化するためのものです。
# FCMは、各データ点が複数のクラスターに所属する可能性を許容する「ファジィ」なクラスタリング手法です。
# 各データ点は、各クラスターに対する「メンバーシップ値」を持ち、この値が高いほど、そのクラスターに強く所属していることを意味します。

# パッケージの読み込み
suppressPackageStartupMessages({
  library(tidyverse) # データ操作のためのライブラリ
  library(e1071) # FCMクラスタリングのためのライブラリ
  library(scales) # グラフのスケール調整のためのライブラリ
})

source("R/utils.R")
source("R/translation_config.R")

#' @title クラスタリング結果の散布図作成
#' @description 入力データ、メンバーシップ行列、クラスター中心点を用いて、クラスタリング結果の散布図を作成します。
#' @param data 入力データ (data.frame)。質問項目(Q*)を含む必要があります。
#' @param membership メンバーシップ行列 (matrix)。各データ点が各クラスターに所属する度合いを示します。
#' @param centers クラスターの中心点 (matrix)。各クラスターの中心座標を示します。
#' @param labels クラスターのラベル (character vector)。各クラスターのラベルです。
#' @param lang 言語コード (character)。グラフのラベルに使用する言語を指定します ("ja" or "en")。
#' @return ggplotオブジェクト。作成された散布図です。
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
        x = analysis_data[, 1], # 1列目をx座標とする
        y = analysis_data[, 2], # 2列目をy座標とする
        cluster = factor(max.col(membership), # メンバーシップ値が最大のクラスターを取得
          levels = 1:length(labels), # クラスターのレベルを設定
          labels = labels # クラスターのラベルを設定
        ),
        membership = apply(membership, 1, max) # 各データ点の最大メンバーシップ値を取得
      )

      # 中心点の準備
      centers_data <- tibble(
        x = centers[, 1], # 1列目をx座標とする
        y = centers[, 2], # 2列目をy座標とする
        cluster = factor(1:length(labels), # クラスターのレベルを設定
          levels = 1:length(labels), # クラスターのレベルを設定
          labels = labels # クラスターのラベルを設定
        )
      )

      # プロット生成
      p <- ggplot(plot_data, aes(x = x, y = y)) +
        geom_point(
          aes(color = cluster, size = membership, alpha = membership) # クラスターごとに色分けし、メンバーシップ値でサイズと透明度を調整
        ) +
        scale_size_continuous(range = c(2, 5)) + # メンバーシップ値に応じた点のサイズを設定
        scale_alpha_continuous(range = c(0.3, 0.8)) + # メンバーシップ値に応じた点の透明度を設定
        geom_point( # クラスター中心点を描画
          data = centers_data,
          aes(x = x, y = y),
          color = "black", # 中心点の色を黒に設定
          size = 4, # 中心点のサイズを設定
          shape = 8 # 中心点の形を星形に設定
        ) +
        labs( # グラフのラベルを設定
          title = get_translation("cluster_title", "plot_labels", lang),
          x = get_translation("Q1", "questions", lang),
          y = get_translation("Q2", "questions", lang),
          color = get_translation("cluster", "cluster_labels", lang),
          size = get_translation("membership", "cluster_labels", lang),
          alpha = get_translation("membership", "cluster_labels", lang)
        ) +
        scale_color_brewer(palette = "Set2") + # 色分けに"Set2"パレットを使用
        theme_minimal(base_size = 14) + # 最小限のテーマを適用し、フォントサイズを設定
        theme( # テーマの詳細設定
          plot.title = element_text(size = rel(1.6), hjust = 0.5),
          axis.title = element_text(size = rel(1.6)),
          axis.text = element_text(size = rel(1.6)),
          legend.title = element_text(size = rel(1.6)),
          legend.text = element_text(size = rel(1.1)),
          legend.position = "bottom",
          legend.box = "vertical",
          panel.grid.minor = element_blank()
        )

      return(p) # 作成したプロットを返す
    },
    error = function(e) { # エラーハンドリング
      warning(sprintf("散布図の作成中にエラー: %s", e$message)) # エラーメッセージを出力
      return(NULL) # NULLを返す
    }
  )
}

#' @title Fuzzy c-means クラスタリング実行と可視化
#' @description Fuzzy c-means (FCM) クラスタリングを実行し、結果をリスト形式で返します。
#' @param data アンケートの生データ (data.frame)。質問項目(Q*)を含む必要があります。
#' @param k クラスター数 (integer)。生成するクラスターの数を指定します。
#' @param m ファジィ化パラメータ (numeric)。メンバーシップ関数の形状を制御するパラメータ (m > 1)。
#'  値が大きいほど、クラスター間の境界が曖昧になります。
#' @param max_iter 最大反復回数 (integer)。FCMアルゴリズムの最大反復回数を指定します。
#' @param lang 言語コード (character)。クラスターラベルに使用する言語を指定します ("ja" or "en")。
#' @return クラスタリング結果のリスト。以下の要素を含みます:
#'  \itemize{
#'   \item clusters: FCMアルゴリズムの結果オブジェクト (cmeansオブジェクト)。
#'   \item labels: クラスターラベル (character vector)。
#'   \item sorted_indices: クラスター中心の平均値でソートされたインデックス (integer vector)。
#'   \item center_means: クラスター中心の平均値 (numeric vector)。
#'  }
perform_clustering <- function(data, k = 4, m = 4, max_iter = 1000, lang = "ja") {
  tryCatch(
    {
      # データの前処理と正規化
      analysis_data <- data %>%
        select(starts_with("Q")) %>% # Qで始まる列を選択
        na.omit() %>% # 欠損値を含む行を除外
        scale() # データを標準化 (平均0、分散1)

      # データの検証
      if (nrow(analysis_data) == 0 || ncol(analysis_data) == 0) {
        stop("前処理後に有効なデータがありません")
      }

      # クラスタリングの実行
      set.seed(123) # 乱数シードを固定し、結果の再現性を確保
      message("\nクラスタリングを開始")

      # Fuzzy C-means クラスタリングの実行
      # FCMアルゴリズムは、以下の手順を繰り返します。
      # 1. 初期化: 各クラスターの中心をランダムに初期化します。
      # 2. メンバーシップ値の計算: 各データ点について、各クラスターへの所属度合い(メンバーシップ値)を計算します。
      #    メンバーシップ値は、データ点とクラスター中心との距離に基づいて計算され、合計は1になります。
      # 3. クラスター中心の更新: 各クラスターの中心を、メンバーシップ値で重み付けされたデータ点の平均として再計算します。
      # 4. 収束判定: クラスター中心の変化が十分に小さくなるか、最大反復回数に達するまで、2と3を繰り返します。
      fuzzy_result <- cmeans(
        analysis_data, # クラスタリング対象のデータ
        centers = k, # クラスター数
        m = m, # ファジィ化パラメータ
        iter.max = max_iter, # 最大反復回数
        dist = "euclidean", # 距離関数 (ユークリッド距離)
        method = "cmeans", # FCMアルゴリズムを指定
        verbose = TRUE # 詳細な情報を出力
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
          sprintf("cluster_%d", i) # 想定外のクラスター数になった場合のラベル
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
        clusters = fuzzy_result, # FCMアルゴリズムの結果オブジェクト
        labels = cluster_labels, # クラスターラベル
        sorted_indices = sorted_indices, # クラスター中心の平均値でソートされたインデックス
        center_means = center_means # クラスター中心の平均値
      )
    },
    error = function(e) { # エラーハンドリング
      message("クラスタリング中にエラー: ", e$message) # エラーメッセージを出力
      return(NULL) # NULLを返す
    }
  )
}

#' @title クラスターごとのVASプロット生成
#' @description 各クラスターについて、VAS (Visual Analog Scale) 値の分布を示すプロットを生成します。
#' @param data 長形式データ (data.frame)。'ID'、'question'、'value' の各列を含む必要があります。
#' @param cluster_info クラスター情報 (data.frame)。'ID' と 'cluster' の各列を含む必要があります。
#'  `cluster_summary` 属性を持っている必要があります。
#' @param plots_dir プロット保存ディレクトリ (character)。生成されたプロットを保存するディレクトリのパス。
#' @param lang 言語コード (character)。グラフのラベルに使用する言語を指定します ("ja" or "en")。
#' @param k クラスター数 (integer)。
#' @return プロットのリスト (list)。各クラスターに対応するggplotオブジェクトを含みます。
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
        mutate(ID = as.character(ID)) %>% # IDを文字列型に変換
        inner_join( # クラスター情報を結合
          cluster_info %>%
            mutate(
              ID = as.character(ID), # IDを文字列型に変換
              cluster = as.factor(cluster) # clusterを因子型に変換
            ) %>%
            select(ID, cluster), # IDとcluster列を選択
          by = "ID" # IDをキーとして結合
        )

      if (nrow(data_with_clusters) == 0) {
        stop("結合後のデータが空です")
      }

      # プロット生成
      plots <- list()
      ordered_clusters <- cluster_summary$cluster # クラスターの順序を取得

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
            geom_boxplot(width = 0.2, alpha = 0.4, outlier.shape = NA) + # 箱ひげ図
            geom_violin(alpha = 0.4) + # バイオリンプロット
            geom_jitter(alpha = 0.4, width = 0.2, height = 0) + # ジッタープロット (データ点を散布)
            labs( # グラフのラベルを設定
              title = sprintf(
                "%s: %s",
                get_translation("cluster", "cluster_labels", lang),
                cluster_label
              ),
              x = get_translation("x_label", "plot_labels", lang),
              y = get_translation("y_label", "plot_labels", lang)
            ) +
            scale_y_continuous( # y軸のスケールを設定
              limits = c(0, 1.00), # 範囲を0から1に設定
              breaks = seq(0, 1.00, 0.20) # 目盛りを0.2刻みに設定
            ) +
            theme_minimal(base_size = 14) + # 最小限のテーマを適用し、フォントサイズを設定
            theme( # テーマの詳細設定
              plot.title = element_text(size = rel(1.6), hjust = 0.5),
              axis.title = element_text(size = rel(1.6)),
              axis.text = element_text(size = rel(1.6)),
              axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid.minor = element_blank(),
              plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
            )

          plots[[cluster_label]] <- p # プロットをリストに追加

          # プロットの保存
          if (!is.null(plots_dir)) {
            safe_name <- gsub(" ", "_", tolower(cluster_label)) # クラスターラベルをファイル名に使える形に変換
            filename_base <- file.path(plots_dir, sprintf("vas_cluster_%s", safe_name))

            tryCatch(
              {
                save_results(p, paste0(filename_base, ".pdf")) # PDF形式で保存
                save_results(p, paste0(filename_base, ".svg")) # SVG形式で保存
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

      return(plots) # プロットのリストを返す
    },
    error = function(e) { # エラーハンドリング
      warning(sprintf("クラスターVASプロットの作成中にエラー: %s", e$message)) # エラーメッセージを出力
      return(NULL) # NULLを返す
    }
  )
}

#' @title メイン実行関数
#' @description Fuzzy C-meansクラスタリングを実行し、結果を可視化・保存するメイン関数です。
#' @param input_file 入力ファイルパス (character)。CSV形式のデータファイルへのパス。
#' @param output_dir 出力ディレクトリ (character)。結果を保存するディレクトリへのパス。
#' @param lang 言語コード (character)。グラフのラベル等に使用する言語 ("ja" or "en")。
#' @param verbose デバッグ情報の表示フラグ (logical)。TRUEの場合、詳細な情報を出力します。
#' @param benchmark ベンチマーク実行フラグ (logical)。TRUEの場合、実行時間を計測します (未実装)。
#' @return 成功時は `invisible(TRUE)`、エラー時は `invisible(FALSE)` を返します。
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
      plots_dir <- file.path(output_dir, "plots") # プロット保存用ディレクトリ
      data_dir <- file.path(output_dir, "data") # データ保存用ディレクトリ
      dirs <- c(plots_dir, data_dir)
      lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE) # ディレクトリを作成

      # データの読み込みと前処理
      data <- load_and_preprocess_data(input_file, lang)
      if (is.null(data)) {
        stop("データの読み込みに失敗しました")
      }

      # クラスター分析の実行と散布図の生成
      k <- 4 # クラスター数
      cluster_results <- perform_clustering(data$raw, k = k, lang = lang) # クラスタリング実行
      if (is.null(cluster_results)) {
        stop("クラスター分析に失敗しました")
      }

      # クラスタリング結果の散布図を生成
      cluster_plot <- create_cluster_plot(
        data = data$raw, # 元データ
        membership = cluster_results$clusters$membership, # メンバーシップ行列
        centers = cluster_results$clusters$centers, # クラスター中心
        labels = cluster_results$labels, # クラスターラベル
        lang = lang # 言語設定
      )

      # 散布図の保存
      if (!is.null(cluster_plot)) {
        save_results(cluster_plot, file.path(plots_dir, "clustering.pdf")) # PDF形式で保存
        save_results(cluster_plot, file.path(plots_dir, "clustering.svg")) # SVG形式で保存
        message("クラスタリング結果の散布図を保存しました")
      } else {
        warning("クラスタリング結果の散布図の生成をスキップしました")
      }

      # クラスター情報の作成
      cluster_info <- tibble(
        ID = data$raw$ID, # ID
        cluster = factor( # クラスター番号 (メンバーシップ値が最大のものを採用)
          max.col(cluster_results$clusters$membership),
          levels = 1:k,
          labels = cluster_results$labels
        ),
        membership = apply(cluster_results$clusters$membership, 1, max) # 最大メンバーシップ値
      )

      # クラスター概要情報を属性として追加
      attr(cluster_info, "cluster_summary") <- tibble(
        cluster = factor(1:k, levels = 1:k, labels = cluster_results$labels), # クラスターラベル
        center_mean = cluster_results$center_means, # クラスター中心の平均値
        sorted_index = cluster_results$sorted_indices # ソートされたインデックス
      )

      # クラスター情報の保存
      save_results(cluster_info, file.path(data_dir, "cluster_info.csv")) # CSV形式で保存

      # クラスターごとのVASプロット生成と保存
      create_cluster_vas_plots(data$long, cluster_info, plots_dir, lang, k)

      message("クラスター分析が完了しました")
      return(invisible(TRUE)) # 成功
    },
    error = function(e) { # エラーハンドリング
      message(sprintf("クラスター分析の実行中にエラー: %s", e$message)) # エラーメッセージを出力
      return(invisible(FALSE)) # 失敗
    }
  )
}
