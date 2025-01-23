#' @title 映画印象分析システムの多言語設定
#' @description
#' 映画の印象評価データの分析に使用する多言語設定を管理するモジュール。
#' システムメッセージ、質問項目、プロットラベル、統計量など、
#' 分析に必要な全ての文字列を日本語と英語で管理します。
#'
#' @author データサイエンス学部
#' @version 1.1.0
#' @date 2025-01-15

library(yaml)

# システムのデフォルトフォント設定
# OSに応じて適切なフォントファミリーを選択します
DEFAULT_FONT_FAMILY <- list(
  "ja" = ifelse(
    Sys.info()["sysname"] == "Windows",
    "MS Gothic",
    "IPAGothic"
  ),
  "en" = ifelse(
    Sys.info()["sysname"] == "Windows",
    "Arial",
    "DejaVu Sans"
  )
)

#' @section 翻訳データの構造:
#' 翻訳データは以下のカテゴリーで構成されています：
#' - system_messages: システム通知やエラーメッセージ
#' - questions: 評価質問項目
#' - plot_labels: グラフやプロットのラベル
#' - stat_columns: 統計情報の列名
#' - cluster_labels: クラスター分析関連のラベル
#' - tabs: UIタブのラベル
#' - emotion_categories: 感情カテゴリーの名称
#' - influence_analysis: 影響分析関連のラベル

# 多言語翻訳設定
# 各言語コードをキーとして、その言語での表示文字列を定義します
TRANSLATIONS <- list(
  "ja" = list(
    # システムメッセージ（エラーや通知など）
    "system_messages" = list(
      "analysis_complete" = "分析が完了しました。出力ファイルは 'output/ja' ディレクトリに保存されています。",
      "error_no_data" = "データファイルが見つかりません。",
      "error_invalid_format" = "データ形式が不正です。",
      "time_series_analysis_start" = "時系列分析を開始します...",
      "time_series_analysis_end" = "時系列分析が完了しました。",
      "data_processing_error" = "データ処理中にエラーが発生しました: %s",
      "no_valid_data" = "有効なデータが見つかりませんでした。",
      "analysis_failed" = "分析の実行に失敗しました。",
      "missing_country_column" = "国別分析に必要な'Country'列がデータに存在しません。",
      "model_error" = "モデルの推定中にエラーが発生しました: %s",
      "plot_creation_error" = "グラフの作成中にエラーが発生しました: %s",
      "visualization_error" = "分析結果の可視化中にエラーが発生しました: %s",
      "save_error" = "ファイルの保存中にエラーが発生しました: %s"
    ),

    # 質問項目（アンケートの各設問）
    "questions" = list(
      "Q1" = "ワクワク感",
      "Q2" = "面白さ",
      "Q3" = "喜び",
      "Q4" = "怒り",
      "Q5" = "観覧意欲"
    ),

    # プロットのラベル（グラフの題名や軸ラベルなど）
    "plot_labels" = list(
      "main_title" = "映画の印象評価（VAS）",
      "cluster_title" = "クラスター分析結果",
      "x_label" = "質問項目",
      "y_label" = "評価値",
      "pc1_label" = "第1主成分",
      "pc2_label" = "第2主成分",
      "time_series_title" = "時系列変化",
      "distribution" = "分布",
      "boxplot" = "箱ひげ図",
      "overall_prefix" = "全体 - ",
      "japan_prefix" = "日本 - ",
      "singapore_prefix" = "シンガポール - ",
      "country" = "国",
      "country_comparison" = "国別比較分析"
    ),

    # 統計量の列名（基本統計量や検定結果など）
    "stat_columns" = list(
      "sample_size" = "サンプルサイズ",
      "mean" = "平均値",
      "sd" = "標準偏差",
      "median" = "中央値",
      "q1" = "第1四分位数",
      "q3" = "第3四分位数",
      "min" = "最小値",
      "max" = "最大値",
      "stats" = "統計情報",
      "cluster_stats" = "クラスター統計",
      "t_stat" = "t値",
      "p_value" = "p値",
      "mean_diff" = "平均値の差",
      "conf_low" = "信頼区間下限",
      "conf_high" = "信頼区間上限"
    ),

    # クラスター関連のラベル
    "cluster_labels" = list(
      "cluster" = "クラスター",
      "high_eval" = "高評価群",
      "mid_eval" = "中評価群",
      "low_eval" = "低評価群",
      "mixed_eval" = "混合群",
      "cluster_stats" = "クラスター統計情報",
      "cluster_analysis" = "クラスター分析",
      "membership" = "メンバーシップ度"
    ),

    # タブ関連のラベル
    "tabs" = list(
      "overview" = "概要",
      "stats" = "統計情報",
      "clusters" = "クラスター分析"
    ),

    # 感情カテゴリーのラベル
    "emotion_categories" = list(
      "excitement" = "ワクワク感",
      "interest" = "面白さ",
      "joy" = "喜び",
      "anger" = "怒り",
      "intention" = "観覧意欲"
    ),

    # 影響分析関連のラベル
    "influence_analysis" = list(
      "influence_title" = "観覧意欲への影響分析",
      "variable" = "評価項目",
      "estimate" = "推定値",
      "std_estimate" = "標準化係数",
      "std_error" = "標準誤差",
      "t_value" = "t値",
      "p_value" = "p値",
      "vif" = "VIF",
      "correlation" = "相関係数",
      "significant" = "有意",
      "effect_size" = "効果量",
      "effect_large" = "大",
      "effect_medium" = "中",
      "effect_small" = "小",
      "model_fit" = "モデル適合度",
      "r_squared" = "決定係数",
      "adj_r_squared" = "自由度調整済み決定係数",
      "f_statistic" = "F統計量",
      "coefficient_plot_title" = "観覧意欲への影響度（標準化係数）",
      "coefficient_plot_subtitle" = "国別分析：%s",
      "correlation_plot_title" = "相関行列ヒートマップ",
      "correlation_plot_subtitle" = "国別分析：%s",
      "axis_variable" = "評価項目",
      "axis_std_coefficient" = "標準化係数",
      "excitement" = "ワクワク感 (Q1)",
      "interest" = "面白さ (Q2)",
      "joy" = "喜び (Q3)",
      "anger" = "怒り (Q4)",
      "intention" = "観覧意欲 (Q5)"
    )
  ),
  "en" = list(
    # System Messages
    "system_messages" = list(
      "analysis_complete" = "Analysis completed. Output files are saved in 'output/en' directory.",
      "error_no_data" = "Data file not found.",
      "error_invalid_format" = "Invalid data format.",
      "time_series_analysis_start" = "Starting time series analysis...",
      "time_series_analysis_end" = "Time series analysis completed.",
      "data_processing_error" = "Error occurred during data processing: %s",
      "no_valid_data" = "No valid data found.",
      "analysis_failed" = "Analysis execution failed.",
      "missing_country_column" = "Country column required for country-specific analysis is missing.",
      "model_error" = "Error occurred during model estimation: %s",
      "plot_creation_error" = "Error occurred while creating plot: %s",
      "visualization_error" = "Error occurred while visualizing analysis results: %s",
      "save_error" = "Error occurred while saving file: %s"
    ),

    # Questions
    "questions" = list(
      "Q1" = "Excitement",
      "Q2" = "Interest",
      "Q3" = "Joy",
      "Q4" = "Anger",
      "Q5" = "Viewing Intention"
    ),

    # Plot Labels
    "plot_labels" = list(
      "main_title" = "Movie Impression Evaluation (VAS)",
      "cluster_title" = "Clustering Results",
      "x_label" = "Questions",
      "y_label" = "Value",
      "pc1_label" = "First Principal Component",
      "pc2_label" = "Second Principal Component",
      "time_series_title" = "Time Series Change",
      "distribution" = "Distribution",
      "boxplot" = "Box Plot",
      "overall_prefix" = "Overall - ",
      "japan_prefix" = "Japan - ",
      "singapore_prefix" = "Singapore - ",
      "country" = "Country",
      "country_comparison" = "Country Comparison Analysis"
    ),

    # Statistical Columns
    "stat_columns" = list(
      "sample_size" = "Sample Size",
      "mean" = "Mean",
      "sd" = "Standard Deviation",
      "median" = "Median",
      "q1" = "1st Quartile",
      "q3" = "3rd Quartile",
      "min" = "Minimum",
      "max" = "Maximum",
      "stats" = "Statistics",
      "cluster_stats" = "Cluster Statistics",
      "t_stat" = "t-statistic",
      "p_value" = "p-value",
      "mean_diff" = "Mean Difference",
      "conf_low" = "CI Lower",
      "conf_high" = "CI Upper"
    ),

    # Cluster Labels
    "cluster_labels" = list(
      "cluster" = "Cluster",
      "high_eval" = "High Evaluation",
      "mid_eval" = "Medium Evaluation",
      "low_eval" = "Low Evaluation",
      "mixed_eval" = "Mixed Evaluation",
      "cluster_stats" = "Cluster Statistics",
      "cluster_analysis" = "Cluster Analysis",
      "membership" = "Membership Degree"
    ),

    # Tabs
    "tabs" = list(
      "overview" = "Overview",
      "stats" = "Statistics",
      "clusters" = "Cluster Analysis"
    ),

    # Emotion Categories
    "emotion_categories" = list(
      "excitement" = "Excitement",
      "interest" = "Interest",
      "joy" = "Joy",
      "anger" = "Anger",
      "intention" = "Viewing Intention"
    ),

    # Influence Analysis
    "influence_analysis" = list(
      "influence_title" = "Analysis of Viewing Intention",
      "variable" = "Variable",
      "estimate" = "Estimate",
      "std_estimate" = "Standardized Coefficient",
      "std_error" = "Std. Error",
      "t_value" = "t-value",
      "p_value" = "p-value",
      "vif" = "VIF",
      "correlation" = "Correlation",
      "significant" = "Significant",
      "effect_size" = "Effect Size",
      "effect_large" = "Large",
      "effect_medium" = "Medium",
      "effect_small" = "Small",
      "model_fit" = "Model Fit",
      "r_squared" = "R-squared",
      "adj_r_squared" = "Adjusted R-squared",
      "f_statistic" = "F-statistic",
      "coefficient_plot_title" = "Impact on Viewing Intention (Standardized Coefficients)",
      "coefficient_plot_subtitle" = "Country Analysis: %s",
      "correlation_plot_title" = "Correlation Matrix Heatmap",
      "correlation_plot_subtitle" = "Country Analysis: %s",
      "axis_variable" = "Variables",
      "axis_std_coefficient" = "Standardized Coefficient",
      "excitement" = "Excitement (Q1)",
      "interest" = "Interest (Q2)",
      "joy" = "Joy (Q3)",
      "anger" = "Anger (Q4)",
      "intention" = "Viewing Intention (Q5)"
    )
  )
)

#' @title 翻訳テキストの取得
#' @description
#' 指定された言語、カテゴリー、キーに対応する翻訳テキストを取得します。
#'
#' @param key 翻訳キー (例: "Q1", "main_title" など)
#' @param category カテゴリー (例: "questions", "plot_labels" など)
#' @param lang 言語コード ("ja" または "en")
#' @return character 翻訳されたテキスト
#' @examples
#' # 日本語の質問1を取得
#' text_ja <- get_translation("Q1", "questions", "ja")
#'
#' # 英語のメインタイトルを取得
#' text_en <- get_translation("main_title", "plot_labels", "en")
#'
#' @export
get_translation <- function(key, category, lang = getOption("movie_analysis.lang", "ja")) {
  if (!lang %in% names(TRANSLATIONS)) {
    stop(sprintf("Unsupported language: %s", lang))
  }
  if (!category %in% names(TRANSLATIONS[[lang]])) {
    stop(sprintf("Unknown category: %s", category))
  }
  if (!key %in% names(TRANSLATIONS[[lang]][[category]])) {
    stop(sprintf("Unknown key: %s in category: %s", key, category))
  }
  TRANSLATIONS[[lang]][[category]][[key]]
}

#' @title 言語設定の変更
#' @description
#' システム全体の表示言語を変更します。
#'
#' @param lang 言語コード ("ja" または "en")
#' @return NULL
#' @examples
#' # 日本語に設定
#' set_language("ja")
#'
#' # 英語に設定
#' set_language("en")
#'
#' @export
set_language <- function(lang = "ja") {
  if (!lang %in% names(TRANSLATIONS)) {
    stop(sprintf("Unsupported language: %s", lang))
  }
  options(movie_analysis.lang = lang)
}

#' @title エラーメッセージの取得
#' @param error_key エラーキー
#' @param lang 言語コード
#' @return character エラーメッセージ
get_error_message <- function(error_key, lang = getOption("movie_analysis.lang", "ja")) {
  tryCatch(
    {
      get_translation(error_key, "system_messages", lang)
    },
    error = function(e) {
      # 翻訳が見つからない場合はエラーキーをそのまま返す
      warning(sprintf("Error message not found for key: %s", error_key))
      error_key
    }
  )
}
