# サンプルデータの生成
source("create_sample_data.R")

# 言語設定 ("ja" または "en" を指定)
lang <- "ja"
data_file <- "sample_data.csv"
output_dir <- file.path("sample_output", lang)

source("basic_trend.R")
# 基本統計量とVASプロットの実行
basic_trend_result <- main_basic_trend(
  input_file = data_file,
  output_dir = output_dir,
  lang = lang,
  verbose = TRUE,
  benchmark = FALSE
)

source("fuzzy_cmeans.R")
# Fuzzy cmeansクラスタリングの実行
fuzzy_cmeans_result <- main_fuzzy_cmeans(
  input_file = data_file,
  output_dir = output_dir,
  lang = lang,
  verbose = TRUE,
  benchmark = FALSE
)

source("viewing_intention.R")
# 視聴意欲分析の実行
viewing_intention_result <- main_viewing_intention(
  input_file = data_file,
  output_dir = output_dir,
  lang = lang,
  verbose = TRUE,
  benchmark = FALSE
)
