# パッケージの読み込み
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggbeeswarm)
})

# utils.Rからヘルパー関数をロード
source("R/utils.R")
source("R/translation_config.R")

#' @title メイン実行関数の改善版
#' @param input_file 入力ファイルパス
#' @param output_dir 出力ディレクトリ
#' @param lang 言語コード
#' @param verbose デバッグ情報の表示フラグ
#' @param benchmark ベンチマーク実行フラグ
main_basic_trend <- function(input_file, output_dir, lang = "ja", verbose = FALSE, benchmark = FALSE) {
  # 初期化処理の開始時刻を記録
  start_time <- Sys.time()

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

  if (verbose) message("VAS分布(全体)の可視化を開始...")
  vas_plot <- create_vas_plot(data$long, lang)
  save_results(vas_plot, file.path(plots_dir, "vas_distribution.pdf"))
  save_results(vas_plot, file.path(plots_dir, "vas_distribution.svg"))

  if (verbose) message("VAS分布(日本)の可視化を開始...")
  vas_plot_japan <- create_vas_plot(data$long, lang, country_filter = "Japan")
  save_results(vas_plot_japan, file.path(plots_dir, "vas_distribution_ja.pdf"))
  save_results(vas_plot_japan, file.path(plots_dir, "vas_distribution_ja.svg"))

  if (verbose) message("VAS分布(シンガポール)の可視化を開始...")
  vas_plot_singapore <- create_vas_plot(data$long, lang, country_filter = "Singapore")
  save_results(vas_plot_singapore, file.path(plots_dir, "vas_distribution_sg.pdf"))
  save_results(vas_plot_singapore, file.path(plots_dir, "vas_distribution_sg.svg"))

  if (verbose) message("VAS分布の国別比較を開始...")
  vas_plots_comparison <- create_vas_plots_comparison(data$long, lang)
  save_results(vas_plots_comparison, file.path(plots_dir, "vas_distribution_comparison.pdf"))
  save_results(vas_plots_comparison, file.path(plots_dir, "vas_distribution_comparison.svg"))

  if (verbose) message("基本統計量(全体)の計算を開始...")
  stats_all <- calculate_basic_stats(data$long, lang)
  if (verbose) message("全体の基本統計量を保存...(basic_stats.csv)")
  save_results(stats_all, file.path(data_dir, "basic_stats.csv"))

  if (verbose) message("日本の基本統計量を計算...")
  stats_japan <- calculate_basic_stats(data$long, lang, country_filter = "Japan")
  if (verbose) message("日本の基本統計量を保存...(basic_stats_ja.csv)")
  save_results(stats_japan, file.path(data_dir, "basic_stats_ja.csv"))

  if (verbose) message("シンガポールの基本統計量を計算...")
  stats_singapore <- calculate_basic_stats(data$long, lang, country_filter = "Singapore")
  save_results(stats_singapore, file.path(data_dir, "basic_stats_sg.csv"))

  if (verbose) message("国別t検定を開始...")
  t_test_results <- perform_country_ttest(data$long, lang)
  save_results(t_test_results, file.path(data_dir, "country_ttest_results.csv"))
}

#' @title 最適化された基本統計量の計算
#' @description 指定された国のデータに対して基本統計量を計算します
#' @param data 長形式データ
#' @param lang 言語コード
#' @param country_filter 特定の国のデータのみを分析する場合の国名（オプション）
#' @return 基本統計量のデータフレーム
calculate_basic_stats <- function(data, lang = "ja", country_filter = NULL) {
  # 国別フィルタリング
  if (!is.null(country_filter)) {
    data <- data %>% filter(Country == country_filter)
  }
  # tidyverseを使用した集計
  stats <- data %>%
    group_by(question) %>%
    summarise(
      n = n(),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      q1 = quantile(value, 0.25, na.rm = TRUE),
      q3 = quantile(value, 0.75, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      .groups = "drop"
    )

  # 列名の変換
  stats <- stats %>%
    rename(
      !!get_translation("x_label", "plot_labels", lang) := question,
      !!get_translation("sample_size", "stat_columns", lang) := n,
      !!get_translation("mean", "stat_columns", lang) := mean,
      !!get_translation("sd", "stat_columns", lang) := sd,
      !!get_translation("median", "stat_columns", lang) := median,
      !!get_translation("q1", "stat_columns", lang) := q1,
      !!get_translation("q3", "stat_columns", lang) := q3,
      !!get_translation("min", "stat_columns", lang) := min,
      !!get_translation("max", "stat_columns", lang) := max
    )

  return(stats)
}

#' @title 最適化されたVASプロットの作成
#' @param data 長形式データ
#' @param lang 言語コード
#' @param country_filter 特定の国のデータのみを表示する場合の国名
#' @return ggplotオブジェクト
create_vas_plot <- function(data, lang = "ja", country_filter = NULL) {
  # 国別フィルタリング
  if (!is.null(country_filter)) {
    data <- data %>% filter(Country == country_filter)
    plot_title <- sprintf(
      "%s (%s)",
      get_translation("main_title", "plot_labels", lang),
      country_filter
    )
  } else {
    plot_title <- get_translation("main_title", "plot_labels", lang)
  }

  # データの事前集計を最適化
  plot_data <- data %>%
    group_by(question) %>%
    summarise(
      value_list = list(value),
      .groups = "drop"
    )

  # より効率的なプロット生成
  p <- ggplot() +
    map(seq_len(nrow(plot_data)), function(i) {
      list(
        # バイオリンプロットを直接生成
        geom_violin(
          data = tibble(
            question = plot_data$question[i],
            value = unlist(plot_data$value_list[i])
          ),
          aes(x = question, y = value),
          alpha = 0.4
        ),
        # 箱ひげ図を生成
        geom_boxplot(
          data = tibble(
            question = plot_data$question[i],
            value = unlist(plot_data$value_list[i])
          ),
          aes(x = question, y = value),
          width = 0.2,
          alpha = 0.4,
          outlier.shape = NA
        )
      )
    }) +
    # より効率的なbeeswarmプロット
    geom_quasirandom(
      data = data,
      aes(x = question, y = value),
      alpha = 0.4,
      size = 1.5,
      width = 0.2,
      groupOnX = TRUE
    ) +
    labs(
      title = plot_title,
      x = get_translation("x_label", "plot_labels", lang),
      y = get_translation("y_label", "plot_labels", lang)
    ) +
    apply_common_theme(rotate_x_labels = TRUE) +
    theme(
      text = element_text(size = 14),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 20),
      plot.title = element_text(size = 18),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 14)
    )

  return(p)
}

#' @title 国別VASプロットの作成
#' @param data 長形式データ
#' @param lang 言語コード
#' @return ggplotオブジェクト
create_vas_plots_comparison <- function(data, lang = "ja") {
  # 国別のバイオリンプロット + 箱ひげ図
  p <- ggplot(data, aes(x = question, y = value, fill = Country)) +
    # バイオリンプロット
    geom_violin(
      position = position_dodge(width = 0.7),
      alpha = 0.5,
      scale = "width",
      trim = TRUE
    ) +
    # 箱ひげ図
    geom_boxplot(
      position = position_dodge(width = 0.7),
      width = 0.2,
      alpha = 0.7,
      outlier.shape = NA # 外れ値は点プロットで表示するため非表示
    ) +
    # 個別データ点
    geom_point(
      position = position_jitterdodge(
        dodge.width = 0.7,
        jitter.width = 0.1
      ),
      alpha = 0.3,
      size = 1
    ) +
    # グレースケールの設定
    scale_fill_grey(start = 0.4, end = 0.8) +
    # ラベル設定
    labs(
      title = get_translation("country_comparison", "plot_labels", lang),
      x = get_translation("x_label", "plot_labels", lang),
      y = get_translation("y_label", "plot_labels", lang),
      fill = get_translation("country", "plot_labels", lang)
    ) +
    # Y軸の範囲設定
    scale_y_continuous(
      limits = c(0, 1.00),
      breaks = seq(0, 1.00, 0.20)
    ) +
    apply_common_theme(rotate_x_labels = TRUE) +
    theme(
      text = element_text(size = 14),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 20),
      plot.title = element_text(size = 18),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.position = "bottom"
    )
  return(p)
}

#' @title 国別のt検定実施
#' @param data 長形式データ
#' @param lang 言語コード
#' @return t検定結果のデータフレーム
perform_country_ttest <- function(data, lang = "ja") {
  # 質問項目ごとにt検定を実施
  questions <- unique(data$question)

  t_results <- map_dfr(questions, function(q) {
    subset_data <- data %>% filter(question == q)

    # Perform t-test
    t_test <- t.test(value ~ Country, data = subset_data)

    # Calculate mean difference
    means <- subset_data %>%
      group_by(Country) %>%
      summarise(mean = mean(value, na.rm = TRUE), .groups = "drop")
    mean_diff <- diff(means$mean)

    # Create results data frame
    tibble(
      question = q,
      t_statistic = t_test$statistic,
      p_value = t_test$p.value,
      mean_diff = mean_diff,
      conf_low = t_test$conf.int[1],
      conf_high = t_test$conf.int[2]
    )
  })

  # Rename columns according to language
  t_results <- t_results %>%
    rename(
      !!get_translation("x_label", "plot_labels", lang) := question,
      !!get_translation("t_stat", "stat_columns", lang) := t_statistic,
      !!get_translation("p_value", "stat_columns", lang) := p_value,
      !!get_translation("mean_diff", "stat_columns", lang) := mean_diff,
      !!get_translation("conf_low", "stat_columns", lang) := conf_low,
      !!get_translation("conf_high", "stat_columns", lang) := conf_high
    )

  return(t_results)
}
