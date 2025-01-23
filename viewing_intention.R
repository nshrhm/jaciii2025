# パッケージの読み込み
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggbeeswarm)
})

# utils.Rからヘルパー関数をロード
source("utils.R")
source("translation_config.R")

#' @title 映画観覧意欲分析のメイン実行関数
#' @description
#' CSVファイルから映画の印象評価データを読み込み、観覧意欲に影響を与える
#' 要因を分析します。環境設定の初期化、データの前処理、統計分析を
#' 一連の流れとして実行します。
#'
#' @param input_file 入力CSVファイルのパス
#' @param output_dir 分析結果の出力先ディレクトリ
#' @param lang 言語設定（"ja"：日本語、"en"：英語）
#' @param verbose デバッグ情報の詳細表示フラグ
#' @param benchmark 処理時間計測フラグ
#' @return NULL
#' @details
#' 以下の手順で分析を実行します：
#' 1. 環境設定の初期化（フォント、ロケール等）
#' 2. 出力ディレクトリの作成
#' 3. データの読み込みと前処理
#' 4. 観覧意欲の要因分析
#'
#' エラー処理：
#' - 環境設定の初期化失敗
#' - データファイルの読み込み失敗
#' - 分析実行時のエラー
main_viewing_intention <- function(input_file, output_dir, lang = "ja", verbose = FALSE, benchmark = FALSE) {
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
  output_dir <- paste0("output/", lang)
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

  # ビューティングインテンションの分析
  # データフレームを取り出して分析を実行
  analysis_results <- analyze_viewing_intention(data$raw, lang = lang)
  if (is.null(analysis_results)) {
    stop(get_error_message("analysis_failed", lang))
  }

  # 結果の処理
  influence_summary <- analysis_results$influence_summary
  model_fit <- analysis_results$model_fit
  correlation_matrix <- analysis_results$correlation_matrix
  model <- analysis_results$model

  # 結果の保存
  save_results(influence_summary, file.path(data_dir, "viewing_intention_influence_summary.csv"))
  save_results(model_fit, file.path(data_dir, "viewing_intention_model_fit.csv"))
  save_results(as.data.frame(correlation_matrix), file.path(data_dir, "viewing_intention_correlation_matrix.csv"))
  saveRDS(model, file.path(data_dir, "viewing_intention_regression_model.rds"))

  # プロット生成
  plots <- list(
    coefficient = create_coefficient_plot(analysis_results, lang),
    correlation = create_correlation_plot(analysis_results, lang)
  )

  # プロットの保存
  save_results(plots$coefficient, file.path(plots_dir, "viewing_intention_coefficient.pdf"))
  save_results(plots$coefficient, file.path(plots_dir, "viewing_intention_coefficient.svg"))
  save_results(plots$correlation, file.path(plots_dir, "viewing_intention_correlation.pdf"))
  save_results(plots$correlation, file.path(plots_dir, "viewing_intention_correlation.svg"))
}

#' @title 係数プロットの生成関数
#' @description
#' 重回帰分析の結果から、各要因の標準化係数を視覚化するプロットを
#' 生成します。係数の大きさと信頼区間を表示し、影響度の比較を
#' 容易にします。
#'
#' @param analysis_results analyze_viewing_intentionの結果リスト
#' @param lang 言語設定（"ja"：日本語、"en"：英語）
#' @return ggplotオブジェクト（係数プロット）
#' @details
#' プロットの特徴：
#' - 標準化係数のポイントプロット
#' - 95%信頼区間の表示
#' - 要因名の翻訳対応
create_coefficient_plot <- function(analysis_results, lang = "ja") {
  # 影響度データを取得
  influence_data <- analysis_results$influence_summary

  # 信頼区間の計算（t分布の95%信頼区間）
  ci_margin <- influence_data$Std_Error * qt(0.975, df = nrow(influence_data) - 1)

  # プロットの生成
  ggplot(influence_data, aes(x = Std_Estimate, y = Variable)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = 3, color = "steelblue") +
    geom_errorbarh(
      aes(
        xmin = Std_Estimate - ci_margin,
        xmax = Std_Estimate + ci_margin
      ),
      height = 0.2,
      color = "steelblue"
    ) +
    labs(
      title = get_translation("coefficient_plot_title", "influence_analysis", lang),
      x = get_translation("axis_variable", "influence_analysis", lang),
      y = get_translation("axis_std_coefficient", "influence_analysis", lang)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12)
    )
}

#' @title 相関プロットの生成関数
#' @description
#' 変数間の相関関係を視覚化するヒートマッププロットを生成します。
#' 各変数ペアの相関係数を色の濃淡で表現し、正負の関係性の強さを
#' 直感的に把握できるようにします。
#'
#' @param analysis_results analyze_viewing_intention()関数から返される分析結果のリスト
#' @param lang 言語設定（"ja"：日本語、"en"：英語）
#' @return ggplotオブジェクト
#'   - 相関係数をヒートマップで表示
#'   - 各セルに相関係数の値を数値で表示
#'   - 軸ラベルは指定言語で表示
#' @details
#' プロットの仕様：
#' - 相関係数の表示範囲：-1から1
#' - 色使い：
#'     - 正の相関：赤色（強いほど濃い）
#'     - 負の相関：青色（強いほど濃い）
#'     - 無相関：白色
#' - 軸の変数名は translation_config.R で定義された翻訳を使用
#'
#' @examples
#' \dontrun{
#' # 日本語での相関プロット生成
#' plot <- create_correlation_plot(analysis_results, lang = "ja")
#' print(plot)
#'
#' # 英語での相関プロット生成
#' plot <- create_correlation_plot(analysis_results, lang = "en")
#' print(plot)
#' }
#'
#' @seealso
#' - \code{\link{analyze_viewing_intention}} - 分析結果の生成
#' - \code{\link{get_translation}} - 翻訳文字列の取得
create_correlation_plot <- function(analysis_results, lang = "ja") {
  # 相関行列を取得
  cor_matrix <- analysis_results$correlation_matrix

  # 変数名を翻訳
  var_names <- c(
    get_translation("excitement", "influence_analysis", lang),
    get_translation("interest", "influence_analysis", lang),
    get_translation("joy", "influence_analysis", lang),
    get_translation("anger", "influence_analysis", lang),
    get_translation("intention", "influence_analysis", lang)
  )

  # データフレームに変換
  cor_df <- cor_matrix %>%
    as.data.frame() %>%
    rownames_to_column("Var1") %>%
    pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation") %>%
    mutate(
      Var1 = factor(Var1, levels = paste0("Q", 1:5), labels = var_names),
      Var2 = factor(Var2, levels = paste0("Q", 1:5), labels = var_names)
    )

  # プロットの生成
  ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    geom_text(aes(label = sprintf("%.2f", Correlation)), size = 3) +
    labs(
      title = get_translation("correlation_plot_title", "influence_analysis", lang),
      x = get_translation("axis_variable", "influence_analysis", lang),
      y = get_translation("axis_variable", "influence_analysis", lang),
      fill = get_translation("correlation", "influence_analysis", lang)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(size = 12)
    )
}

#' @title 観覧意欲への影響要因の統計分析
#' @description
#' 映画の印象評価データを用いて、観覧意欲（Q5）に対する
#' 各評価項目（Q1-Q4）の影響度を分析します。
#'
#' @param data 分析用データフレーム（Q1-Q5の列を含む）
#' @param subset_country 国別分析を行う場合の対象国名
#' @param lang 出力言語設定（"ja"：日本語、"en"：英語）
#' @return list 以下の要素を含む分析結果のリスト
#' \describe{
#'   \item{influence_summary}{各要因の影響度サマリー}
#'   \item{model_fit}{モデルの適合度指標}
#'   \item{model}{回帰分析モデル}
#'   \item{correlation_matrix}{相関行列}
#' }
#' @details
#' 分析手順：
#' 1. データの妥当性検証
#' 2. 前処理（欠損値の除去、スケーリング）
#' 3. 重回帰分析の実行
#' 4. 標準化係数の算出
#' 5. 相関分析
#' 6. VIF（多重共線性）の確認
analyze_viewing_intention <- function(data, subset_country = NULL, lang = "en") {
  message("入力データの検証を開始")
  if (is.null(data)) {
    warning(get_error_message("no_valid_data", lang))
    return(NULL)
  }

  message("データの前処理を開始")
  model_data <- tryCatch(
    {
      if (!is.null(subset_country)) {
        if (!"Country" %in% names(data)) {
          warning(get_error_message("missing_country_column", lang))
          return(NULL)
        }
        data <- data %>% filter(Country == subset_country)
      }

      data %>% select(all_of(paste0("Q", 1:5)))
    },
    error = function(e) {
      warning(sprintf(get_error_message("data_processing_error", lang), e$message))
      return(NULL)
    }
  )

  if (is.null(model_data) || nrow(model_data) == 0) {
    warning(get_error_message("no_valid_data", lang))
    return(NULL)
  }

  # 標準化データの作成
  message("標準化分析を開始")
  scaled_data <- scale(model_data)
  colnames(scaled_data) <- colnames(model_data)

  # 重回帰分析の実行
  message("重回帰分析を開始")
  model <- tryCatch(
    {
      lm(Q5 ~ Q1 + Q2 + Q3 + Q4, data = model_data)
    },
    error = function(e) {
      warning(sprintf(get_error_message("model_error", lang), e$message))
      return(NULL)
    }
  )

  if (is.null(model)) {
    return(NULL)
  }

  # 標準化データでの重回帰分析
  scaled_model <- lm(scaled_data[, "Q5"] ~ scaled_data[, c("Q1", "Q2", "Q3", "Q4")])

  # 相関行列の計算
  message("相関行列の計算を開始")
  cor_matrix <- cor(model_data)

  # VIFの計算
  message("VIFの計算を開始")
  vif_values <- car::vif(model)

  # 影響度の分析結果をまとめる
  message("影響度の分析結果を作成")
  influence_summary <- tryCatch(
    {
      var_names <- c(
        get_translation("excitement", "influence_analysis", lang),
        get_translation("interest", "influence_analysis", lang),
        get_translation("joy", "influence_analysis", lang),
        get_translation("anger", "influence_analysis", lang)
      )

      coef_table <- summary(model)$coefficients[-1, ]
      scaled_coef <- coef(scaled_model)[-1]

      result_df <- tibble(
        Variable = var_names,
        Estimate = unname(coef_table[, "Estimate"]),
        Std_Estimate = unname(scaled_coef),
        Std_Error = unname(coef_table[, "Std. Error"]),
        t_value = unname(coef_table[, "t value"]),
        p_value = unname(coef_table[, "Pr(>|t|)"]),
        VIF = unname(vif_values),
        Correlation = unname(cor_matrix[1:4, "Q5"])
      )

      # message("作成された影響度データフレーム:", toString(result_df))
      result_df
    },
    error = function(e) {
      warning(sprintf(get_error_message("influence_summary_error", lang), e$message))
      return(NULL)
    }
  )

  if (is.null(influence_summary)) {
    return(NULL)
  }

  # モデル適合度の計算
  message("モデル適合度の計算を開始")
  model_fit <- tibble(
    R_squared = summary(model)$r.squared,
    Adj_R_squared = summary(model)$adj.r.squared,
    F_statistic = summary(model)$fstatistic[1],
    p_value = pf(
      summary(model)$fstatistic[1],
      summary(model)$fstatistic[2],
      summary(model)$fstatistic[3],
      lower.tail = FALSE
    )
  )

  message("分析が完了しました")

  list(
    influence_summary = influence_summary,
    model_fit = model_fit,
    model = model,
    correlation_matrix = cor_matrix
  )
}
