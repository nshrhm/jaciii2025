# パッケージの読み込み
suppressPackageStartupMessages({
  library(tidyverse)
})

#' @title サンプルデータの生成
#' @description クラスタリング結果を反映したサンプルデータを生成します
#' @param n_samples_per_country 各国のサンプル数（デフォルト：30）
#' @return 生成されたサンプルデータのデータフレーム
create_sample_data <- function(n_samples_per_country = 30) {
  # クラスターの特性を定義
  clusters <- list(
    high_eval = list(
      mean = 0.8,
      sd = 0.1,
      weight = 0.3
    ),
    mid_eval = list(
      mean = 0.6,
      sd = 0.1,
      weight = 0.3
    ),
    mixed_eval = list(
      mean = 0.5,
      sd = 0.2,
      weight = 0.2
    ),
    low_eval = list(
      mean = 0.3,
      sd = 0.1,
      weight = 0.2
    )
  )

  # 国ごとのデータ生成
  generate_country_data <- function(country, n_samples) {
    # クラスターの割り当て
    cluster_assignments <- sample(
      names(clusters),
      size = n_samples,
      prob = sapply(clusters, function(x) x$weight),
      replace = TRUE
    )

    # データフレームの作成
    data <- map_dfr(1:n_samples, function(i) {
      cluster <- clusters[[cluster_assignments[i]]]

      # 各質問の値を生成
      values <- pmin(pmax(rnorm(5, mean = cluster$mean, sd = cluster$sd), 0), 1)

      tibble(
        ID = sprintf("%s_%03d", country, i),
        Q1 = values[1],
        Q2 = values[2],
        Q3 = values[3],
        Q4 = values[4],
        Q5 = values[5],
        Country = country
      )
    })

    return(data)
  }

  # 日本とシンガポールのデータを生成
  data_japan <- generate_country_data("Japan", n_samples_per_country)
  data_singapore <- generate_country_data("Singapore", n_samples_per_country)

  # データを結合
  data_all <- bind_rows(data_japan, data_singapore)

  return(data_all)
}

# メイン処理
main <- function() {
  # 乱数のシードを設定
  set.seed(123)

  # サンプルデータの生成（各国30サンプル）
  sample_data <- create_sample_data(30)

  # CSVファイルとして保存
  write_csv(sample_data, "sample_data.csv")

  # サマリー統計量の表示
  cat("\n=== サンプルデータの基本統計量 ===\n")
  print(summary(sample_data[, c("Q1", "Q2", "Q3", "Q4", "Q5")]))

  # 国別サンプル数の確認
  cat("\n=== 国別サンプル数 ===\n")
  print(table(sample_data$Country))
}

# スクリプトの実行
main()
