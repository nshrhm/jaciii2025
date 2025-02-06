# パッケージの読み込み
# ここでは、Rでデータ分析や可視化を行うためのライブラリ（パッケージ）を読み込んでいます。
# suppressPackageStartupMessages() は、パッケージ読み込み時のメッセージを非表示にする関数です。
suppressPackageStartupMessages({
  # tidyverse: データ操作、変換、可視化など、データサイエンスに必要な多くの機能を提供するパッケージ群です。
  #            dplyr, ggplot2, tidyr, readr, purrr, tibble, stringr, forcats などのパッケージが含まれています。
  #            tidyverse は、データ分析のワークフローを効率化するための統一されたインターフェースを提供します。
  library(tidyverse)
  # ggbeeswarm: データの分布を可視化する際に、点が重ならないように描画するためのパッケージです。
  #             geom_quasirandom() 関数などで使用され、データの重なりを避けて視覚化できます。
  library(ggbeeswarm)
})
# Rの基本的なライブラリ
# stats: 基本的な統計関数（平均、分散、検定など）を提供します。
# graphics: 基本的なグラフ描画関数（散布図、ヒストグラムなど）を提供します。
# grDevices: グラフィックデバイス（画面表示、PDF出力など）を制御します。
# utils: 様々なユーティリティ関数（データの読み込み、ヘルプの表示など）を提供します。
# methods: S4オブジェクトシステムを扱うための関数を提供します。
# これらのライブラリは、Rの基本的な機能を提供し、多くのパッケージで利用されています。

# utils.Rからヘルパー関数をロード
# source() 関数は、指定されたRスクリプトファイル（ここでは utils.R と translation_config.R）を読み込み、
# その中に定義されている関数や変数を現在の環境で使用できるようにします。
# utils.R には、データの前処理や可視化、結果の保存など、このスクリプト全体で使われる便利な関数が定義されています。
source("R/utils.R")
# translation_config.R には、グラフのラベルやメッセージを日本語と英語で切り替えるための設定が記述されています。
source("R/translation_config.R")

#' @title メイン実行関数の改善版
#' @description この関数は、指定された入力ファイルを読み込み、データの前処理、可視化、統計分析を行い、結果を指定された出力ディレクトリに保存します。
#' @param input_file 入力ファイルパス (例: "data/data_all.csv")。CSV形式のデータを想定しています。
#' @param output_dir 出力ディレクトリ (例: "output/ja")。結果（グラフやデータ）を保存するディレクトリです。
#' @param lang 言語コード ("ja" (日本語) または "en" (英語))。グラフのラベルやメッセージの言語を指定します。
#' @param verbose デバッグ情報の表示フラグ (TRUE で詳細な情報を表示)。TRUEにすると、処理の詳細な情報がコンソールに出力されます。
#' @param benchmark ベンチマーク実行フラグ (TRUE で実行時間を計測)。TRUEにすると、各処理の実行時間が計測され、コンソールに出力されます。
main_basic_trend <- function(input_file, output_dir, lang = "ja", verbose = FALSE, benchmark = FALSE) {
  # 初期化処理の開始時刻を記録
  start_time <- Sys.time()

  # 環境設定の初期化
  # verbose=TRUE: デバッグ情報を表示
  # install_packages=TRUE: 必要なパッケージを自動インストール
  # force_cairo=FALSE: 必要な場合のみCairoを使用
  # initialize_environment() 関数は、Rの実行環境を初期化する関数です (utils.R で定義されています)。
  # tryCatch() 関数は、エラーが発生した場合の処理を記述するための関数です。
  init_result <- tryCatch(
    {
      initialize_environment(verbose = TRUE)
    },
    error = function(e) {
      # エラーが発生した場合、メッセージを表示してNULLを返します。
      message("環境設定の初期化中にエラーが発生しました: ", e$message)
      return(NULL)
    }
  )

  # 初期化処理の終了時刻を記録
  end_time <- Sys.time()

  # 初期化結果の確認とログ出力
  if (!is.null(init_result)) {
    # 処理時間の計算と表示
    # difftime() 関数は、2つの時刻の差を計算します。
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
    # 初期化に失敗した場合、エラーメッセージを表示してプログラムを停止します。
    stop("環境設定の初期化に失敗しました")
  }

  # 出力ディレクトリの作成
  if (verbose) message("必要なディレクトリを作成...")
  # 出力ディレクトリ構造を設定
  # file.path() 関数は、OSに依存しない形式でファイルパスを作成します。
  plots_dir <- file.path(output_dir, "plots") # グラフを保存するディレクトリ
  data_dir <- file.path(output_dir, "data") # データを保存するディレクトリ
  # 作成するディレクトリのリスト
  dirs <- c(plots_dir, data_dir)
  # lapply() 関数は、リストの各要素に対して指定された関数を適用します。
  # ここでは、dir.create() 関数を使ってディレクトリを作成しています。
  # recursive = TRUE は、親ディレクトリが存在しない場合にそれも作成することを意味します。
  # showWarnings = FALSE は、ディレクトリが既に存在する場合に警告を表示しないようにします。
  lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

  # データの読み込みと前処理
  if (verbose) message("データの読み込みと前処理を開始...")
  # load_and_preprocess_data() 関数は、入力ファイルを読み込んで前処理を行う関数です (utils.R で定義されています)。
  data <- load_and_preprocess_data(input_file, lang)

  # データの読み込みに失敗した場合、メッセージを表示してNULLを返します。
  if (is.null(data)) {
    message("データの読み込みに失敗しました")
    return(NULL)
  }

  # VAS分布の可視化（全体）
  if (verbose) message("VAS分布(全体)の可視化を開始...")
  # create_vas_plot() 関数は、VAS（Visual Analogue Scale）の分布を可視化するグラフを作成する関数です (後述)。
  vas_plot <- create_vas_plot(data$long, lang)
  # save_results() 関数は、グラフやデータをファイルに保存する関数です (utils.R で定義されています)。
  save_results(vas_plot, file.path(plots_dir, "vas_distribution.pdf")) # PDF形式で保存
  save_results(vas_plot, file.path(plots_dir, "vas_distribution.svg")) # SVG形式で保存

  # VAS分布の可視化（日本）
  if (verbose) message("VAS分布(日本)の可視化を開始...")
  # create_vas_plot() 関数を、国を日本に限定して実行します。
  vas_plot_japan <- create_vas_plot(data$long, lang, country_filter = "Japan")
  save_results(vas_plot_japan, file.path(plots_dir, "vas_distribution_ja.pdf"))
  save_results(vas_plot_japan, file.path(plots_dir, "vas_distribution_ja.svg"))

  # VAS分布の可視化（シンガポール）
  if (verbose) message("VAS分布(シンガポール)の可視化を開始...")
  # create_vas_plot() 関数を、国をシンガポールに限定して実行します。
  vas_plot_singapore <- create_vas_plot(data$long, lang, country_filter = "Singapore")
  save_results(vas_plot_singapore, file.path(plots_dir, "vas_distribution_sg.pdf"))
  save_results(vas_plot_singapore, file.path(plots_dir, "vas_distribution_sg.svg"))

  # VAS分布の国別比較
  if (verbose) message("VAS分布の国別比較を開始...")
  # create_vas_plots_comparison() 関数は、国別のVAS分布を比較するグラフを作成する関数です (後述)。
  vas_plots_comparison <- create_vas_plots_comparison(data$long, lang)
  save_results(vas_plots_comparison, file.path(plots_dir, "vas_distribution_comparison.pdf"))
  save_results(vas_plots_comparison, file.path(plots_dir, "vas_distribution_comparison.svg"))

  # 基本統計量の計算（全体）
  if (verbose) message("基本統計量(全体)の計算を開始...")
  # calculate_basic_stats() 関数は、基本統計量（平均値、標準偏差など）を計算する関数です (後述)。
  stats_all <- calculate_basic_stats(data$long, lang)
  if (verbose) message("全体の基本統計量を保存...(basic_stats.csv)")
  save_results(stats_all, file.path(data_dir, "basic_stats.csv"))

  # 基本統計量の計算（日本）
  if (verbose) message("日本の基本統計量を計算...")
  # calculate_basic_stats() 関数を、国を日本に限定して実行します。
  stats_japan <- calculate_basic_stats(data$long, lang, country_filter = "Japan")
  if (verbose) message("日本の基本統計量を保存...(basic_stats_ja.csv)")
  save_results(stats_japan, file.path(data_dir, "basic_stats_ja.csv"))

  # 基本統計量の計算（シンガポール）
  if (verbose) message("シンガポールの基本統計量を計算...")
  # calculate_basic_stats() 関数を、国をシンガポールに限定して実行します。
  stats_singapore <- calculate_basic_stats(data$long, lang, country_filter = "Singapore")
  save_results(stats_singapore, file.path(data_dir, "basic_stats_sg.csv"))

  # 国別t検定
  if (verbose) message("国別t検定を開始...")
  # perform_country_ttest() 関数は、国別のt検定を行う関数です (後述)。
  t_test_results <- perform_country_ttest(data$long, lang)
  save_results(t_test_results, file.path(data_dir, "country_ttest_results.csv"))
}

#' @title 最適化された基本統計量の計算
#' @description 指定された国のデータに対して基本統計量を計算します
#' @param data 長形式データ
#' @param lang 言語コード ("ja" または "en")
#' @param country_filter 特定の国のデータのみを分析する場合の国名（オプション）
#' @return 基本統計量のデータフレーム
calculate_basic_stats <- function(data, lang = "ja", country_filter = NULL) {
  # 国別フィルタリング
  # country_filter が指定されている場合、指定された国のデータのみを抽出します。
  if (!is.null(country_filter)) {
    # filter() 関数は、dplyr パッケージの関数で、条件に一致する行を抽出します。
    # ここでは、Country 列が country_filter で指定された値と一致する行のみを抽出しています。
    data <- data %>% filter(Country == country_filter)
  }

  # tidyverseを使用した集計
  # ここでは、tidyverse の dplyr パッケージの関数を使って、データを質問ごとにグループ化し、基本統計量を計算しています。
  # %>% はパイプ演算子で、左側のオブジェクトを右側の関数の第一引数として渡します。
  stats <- data %>%
    # group_by() 関数は、dplyr パッケージの関数で、指定された列（ここでは question）の値が同じ行をグループ化します。
    group_by(question) %>%
    # summarise() 関数は、dplyr パッケージの関数で、各グループに対して要約統計量を計算します。
    summarise(
      # n() は、各グループの行数（サンプルサイズ）を計算します。
      n = n(),
      # mean() は、各グループの value 列の平均値を計算します。na.rm = TRUE は、欠損値（NA）を除外して計算することを意味します。
      mean = mean(value, na.rm = TRUE),
      # sd() は、各グループの value 列の標準偏差を計算します。
      sd = sd(value, na.rm = TRUE),
      # median() は、各グループの value 列の中央値を計算します。
      median = median(value, na.rm = TRUE),
      # quantile() は、各グループの value 列の四分位数を計算します。0.25 は第1四分位数、0.75 は第3四分位数を表します。
      q1 = quantile(value, 0.25, na.rm = TRUE),
      q3 = quantile(value, 0.75, na.rm = TRUE),
      # min() は、各グループの value 列の最小値を計算します。
      min = min(value, na.rm = TRUE),
      # max() は、各グループの value 列の最大値を計算します。
      max = max(value, na.rm = TRUE),
      # .groups = "drop" は、グループ化を解除します。summarise() の後で group_by() の効果をなくすために使われます。
      .groups = "drop"
    )

  # 列名の変換
  # rename() 関数は、dplyr パッケージの関数で、列名を変更します。
  # !!get_translation(...) := ... の部分は、列名を動的に変更するための記法です。
  # get_translation() 関数は、指定された言語に対応する翻訳を取得します (translation_config.R で定義されています)。
  stats <- stats %>%
    rename(
      # !!get_translation("x_label", "plot_labels", lang) := question は、
      # question 列の名前を、get_translation("x_label", "plot_labels", lang) の戻り値に変更します。
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
#' @description この関数は、VAS（Visual Analogue Scale）のデータを用いて、質問項目ごとの回答分布を可視化するグラフ（ggplotオブジェクト）を作成します。
#' @param data 長形式データ (各行が1つの回答に対応し、question, value, Country などの列を持つ)
#' @param lang 言語コード ("ja" または "en")
#' @param country_filter 特定の国のデータのみを表示する場合の国名 (例: "Japan")
#' @return ggplotオブジェクト (このオブジェクトを print() 関数で表示したり、ggsave() 関数で保存したりできます)
create_vas_plot <- function(data, lang = "ja", country_filter = NULL) {
  # 国別フィルタリング
  # country_filter が指定されている場合、指定された国のデータのみを抽出します。
  if (!is.null(country_filter)) {
    # dplyr の filter() 関数を使って、Country 列が指定された国名と一致する行のみを抽出します。
    data <- data %>% filter(Country == country_filter)
    # プロットのタイトルを設定します。
    plot_title <- sprintf(
      "%s (%s)",
      get_translation("main_title", "plot_labels", lang), # 国名なしのタイトル
      country_filter # 国名
    )
  } else {
    # 国が指定されていない場合は、国名なしのタイトルを使用します。
    plot_title <- get_translation("main_title", "plot_labels", lang)
  }

  # データの事前集計を最適化
  # ここでは、プロットを効率的に生成するために、データを質問項目ごとにグループ化し、各質問に対する回答値のリストを作成しています。
  plot_data <- data %>%
    # dplyr の group_by() 関数を使って、question 列の値でデータをグループ化します。
    group_by(question) %>%
    # dplyr の summarise() 関数を使って、各グループに対して value 列の値をリストとしてまとめます。
    summarise(
      value_list = list(value), # 各質問に対する回答値をリストとして格納
      .groups = "drop" # グループ化を解除
    )

  # より効率的なプロット生成
  # ggplot2 は、Rでグラフを作成するための非常に強力なライブラリです。
  # ggplot2 では、データ、マッピング（データと視覚要素の対応関係）、ジオメトリ（グラフの種類）などを組み合わせてグラフを作成します。
  # "+" 演算子を使って、グラフの要素を層のように重ねていくことができます。
  p <- ggplot() + # 空のggplotオブジェクトを作成
    # map() 関数を使って、各質問項目に対してバイオリンプロットと箱ひげ図を生成します。
    map(seq_len(nrow(plot_data)), function(i) {
      list(
        # バイオリンプロットを直接生成
        # geom_violin() は、ggplot2 の関数で、バイオリンプロットを描画します。
        # バイオリンプロットは、データの分布を滑らかに表現するグラフです。
        geom_violin(
          data = tibble( # 各質問項目に対するデータを作成
            question = plot_data$question[i],
            value = unlist(plot_data$value_list[i]) # リストを展開してベクトルにする
          ),
          aes(x = question, y = value), # x軸に質問項目、y軸に回答値をマッピング
          alpha = 0.4 # プロットの透明度を0.4に設定
        ),
        # 箱ひげ図を生成
        # geom_boxplot() は、ggplot2 の関数で、箱ひげ図を描画します。
        # 箱ひげ図は、データの中央値、四分位数、外れ値を表示するグラフです。
        geom_boxplot(
          data = tibble( # 各質問項目に対するデータを作成
            question = plot_data$question[i],
            value = unlist(plot_data$value_list[i])
          ),
          aes(x = question, y = value),
          width = 0.2, # 箱の幅を0.2に設定
          alpha = 0.4,
          outlier.shape = NA # 外れ値を表示しない
        )
      )
    }) +
    # より効率的なbeeswarmプロット
    # geom_quasirandom() は、beeswarmプロット（点が重ならないように配置された散布図）を描画する関数です。
    # ggbeeswarm パッケージの関数です。
    geom_quasirandom(
      data = data, # 元のデータを使用
      aes(x = question, y = value),
      alpha = 0.4, # プロットの透明度を0.4に設定
      size = 1.5, # 点のサイズを1.5に設定
      width = 0.2, # 点の広がり具合を調整
      groupOnX = TRUE # x軸方向にグループ化
    ) +
    # ラベル設定
    # labs() 関数は、ggplot2 の関数で、グラフのタイトルや軸ラベルを設定します。
    labs(
      title = plot_title, # グラフのタイトル
      x = get_translation("x_label", "plot_labels", lang), # x軸のラベル
      y = get_translation("y_label", "plot_labels", lang) # y軸のラベル
    ) +
    # 共通テーマの適用
    # apply_common_theme() 関数は、グラフの見た目を整えるための共通のテーマを適用します (utils.R で定義されています)。
    apply_common_theme(rotate_x_labels = TRUE) + # x軸のラベルを回転
    # テーマの詳細設定
    # theme() 関数は、ggplot2 の関数で、グラフの細かい見た目を調整します。
    theme(
      text = element_text(size = 14), # 全体のフォントサイズを14に設定
      axis.title = element_text(size = 20), # 軸タイトルのフォントサイズを20に設定
      axis.text = element_text(size = 20), # 軸ラベルのフォントサイズを20に設定
      plot.title = element_text(size = 18), # グラフタイトルのフォントサイズ
      legend.title = element_text(size = 14), # 凡例タイトルのフォントサイズ
      legend.text = element_text(size = 14) # 凡例のフォントサイズ
    )

  return(p)
}

#' @title 国別VASプロットの作成
#' @description この関数は、国ごとにVASの値を比較するプロットを作成します。
#' @param data 長形式データ
#' @param lang 言語コード
#' @return ggplotオブジェクト
create_vas_plots_comparison <- function(data, lang = "ja") {
  # 国別のバイオリンプロット + 箱ひげ図 + データ点
  # ggplot2 を使用してグラフを作成します。
  p <- ggplot(data, aes(x = question, y = value, fill = Country)) +
    # geom_violin() は、バイオリンプロットを描画します。
    # position_dodge() は、複数のグループがある場合に、それぞれのバイオリンプロットを少しずらして表示します。
    geom_violin(
      position = position_dodge(width = 0.7),
      alpha = 0.5, # 透明度
      scale = "width", # 各バイオリンプロットの幅を、そのグループのデータ数に合わせて調整
      trim = TRUE # バイオリンプロットの両端を、データの範囲に合わせる
    ) +
    # geom_boxplot() は、箱ひげ図を描画します。
    geom_boxplot(
      position = position_dodge(width = 0.7), # 箱ひげ図もずらして表示
      width = 0.2, # 箱の幅
      alpha = 0.7,
      outlier.shape = NA # 外れ値は点プロットで表示するため非表示
    ) +
    # geom_point() は、データ点を描画します。
    # position_jitterdodge() は、データ点を少しずらして、重なりを避けるように配置します。
    geom_point(
      position = position_jitterdodge(
        dodge.width = 0.7,
        jitter.width = 0.1
      ),
      alpha = 0.3,
      size = 1
    ) +
    # scale_fill_grey() は、グラフの塗りつぶし色をグレースケールに設定します。
    scale_fill_grey(start = 0.4, end = 0.8) +
    # labs() は、グラフのタイトルや軸ラベルを設定します。
    labs(
      title = get_translation("country_comparison", "plot_labels", lang),
      x = get_translation("x_label", "plot_labels", lang),
      y = get_translation("y_label", "plot_labels", lang),
      fill = get_translation("country", "plot_labels", lang)
    ) +
    # scale_y_continuous() は、y軸の範囲や目盛りの間隔を設定します。
    scale_y_continuous(
      limits = c(0, 1.00),
      breaks = seq(0, 1.00, 0.20)
    ) +
    # apply_common_theme() は、グラフに共通のテーマを適用します (utils.R で定義)。
    apply_common_theme(rotate_x_labels = TRUE) +
    # theme() は、グラフの細かい見た目を調整します。
    theme(
      text = element_text(size = 14),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 20),
      plot.title = element_text(size = 18),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.position = "bottom" # 凡例を下に表示
    )
  return(p)
}

#' @title 国別のt検定実施
#' @description この関数は、2つの国間で、各質問項目に対する回答の平均値に差があるかどうかをt検定で調べます。
#' @param data 長形式データ
#' @param lang 言語コード
#' @return t検定結果のデータフレーム
perform_country_ttest <- function(data, lang = "ja") {
  # 質問項目ごとにt検定を実施
  # unique() 関数で、質問項目の一意なリストを取得します。
  questions <- unique(data$question)

  # map_dfr() 関数は、purrr パッケージの関数で、リストの各要素に関数を適用し、結果を1つのデータフレームにまとめて返します。
  t_results <- map_dfr(questions, function(q) {
    # filter() 関数で、現在の質問項目 q に対応するデータのみを抽出します。
    subset_data <- data %>% filter(question == q)

    # t.test() 関数は、Rの組み込み関数で、t検定を行います。
    # value ~ Country は、Country 変数の値によって value 変数をグループ分けして比較することを意味します。
    t_test <- t.test(value ~ Country, data = subset_data)

    # 各国の平均値を計算します。
    means <- subset_data %>%
      group_by(Country) %>%
      summarise(mean = mean(value, na.rm = TRUE), .groups = "drop")
    # 平均値の差を計算します。
    mean_diff <- diff(means$mean)

    # tibble() 関数は、tibble（tidyverse のデータフレーム）を作成します。
    tibble(
      question = q, # 質問項目
      t_statistic = t_test$statistic, # t統計量
      p_value = t_test$p.value, # p値
      mean_diff = mean_diff, # 平均値の差
      conf_low = t_test$conf.int[1], # 95%信頼区間の下限
      conf_high = t_test$conf.int[2] # 95%信頼区間の上限
    )
  })

  # 列名を、translation_config.R で定義された翻訳を使って変更します。
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
