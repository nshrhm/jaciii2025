# jaciii2025

このリポジトリは、映画評価データの分析を行うRプロジェクトです。Visual Analog Scale (VAS)を用いた映画の印象評価データを分析し、視覚化を行います。

## インストール方法

1. **プロジェクトのダウンロード**
   - コマンドでのインストール
   ```bash
   # GitHubからプロジェクトをクローン
   git clone https://github.com/nshrhm/jaciii2025.git
   ```
   - ブラウザからダウンロード
      - GitHubの「Code」ボタン → 「Download ZIP」からZIPファイルをダウンロードして解凍

   - R環境でのインストール(非推奨)
   ```r
   devtools::install_github("nshrhm/jaciii2025")
   ```

3. **必要なファイル**
   以下のファイルが必要です：
   - `book.pdf`: プロジェクトの解説書
   - `jaciii2025.pdf`: パッケージの詳細マニュアル（英語）
   - `R/basic_trend.R`: 基本統計分析用スクリプト
   - `R.fuzzy_cmeans.R`: クラスター分析用スクリプト
   - `R/viewing_intention.R`: 視聴意欲分析用スクリプト
   - `R/utils.R`: ユーティリティ関数
   - `R/translation_config.R`: 翻訳設定
   - `R/create_sample_data.R`: サンプルデータ生成スクリプト
   - `build_sample.R`: メインの実行スクリプト

## 概要

本プロジェクトでは、以下の3つの主要な分析を行います：

1. **基本統計分析** (`basic_trend.R`)
   - VAS評価の基本統計量（平均、標準偏差など）の算出
   - 日本とシンガポールの比較分析
   - 分布の可視化（バイオリンプロット、箱ひげ図）

2. **クラスター分析** (`fuzzy_cmeans.R`)
   - ファジィc-meansクラスタリングによる回答パターンの分類
   - 4つの評価グループ（高評価群、中評価群、低評価群、混合群）の特徴分析
   - クラスターごとの評価傾向の可視化

3. **視聴意欲分析** (`viewing_intention.R`)
   - 映画の視聴意欲に影響を与える要因の分析
   - 重回帰分析による要因の影響度の算出
   - 相関分析と視覚化

## 使い方

1. **サンプルデータによる動作確認**
   - `create_sample_data.R` が自動的にサンプルデータを生成します
   - 生成されるデータには各質問項目（Q1〜Q5）の評価値と国（Japan/Singapore）が含まれます
   - クラスタリング結果を反映した特性を持つデータが生成されます

2. **分析の実行**
   ```R
   # build_sample.Rを実行してサンプルデータの分析を行います
   source("build_sample.R")
   ```

3. **出力結果**
   - `sample_output/ja/` (日本語) または `sample_output/en/` (英語) に以下の結果が出力されます：
     - `plots/`: 各種グラフ（PDF・SVG形式）
     - `data/`: 分析結果のCSVファイル

## 出力ファイルの説明

### プロット
- `vas_distribution.pdf`: 全体のVAS分布
- `vas_distribution_ja.pdf`: 日本のVAS分布
- `vas_distribution_sg.pdf`: シンガポールのVAS分布
- `vas_distribution_comparison.pdf`: 日本・シンガポールの比較
- `clustering.pdf`: クラスター分析結果
- `viewing_intention_coefficient.pdf`: 視聴意欲への影響係数
- `viewing_intention_correlation.pdf`: 変数間の相関関係

### データファイル
- `basic_stats.csv`: 基本統計量
- `cluster_info.csv`: クラスター分析結果
- `viewing_intention_influence_summary.csv`: 視聴意欲への影響度分析結果

## 必要なパッケージ

以下のRパッケージが必要です：
- tidyverse: データ処理と可視化（ggplot2, dplyr, purrr, tibbleを含む）
- ggbeeswarm: 分布プロットの作成
- e1071: ファジィクラスタリング
- scales: スケール変換
- cluster: クラスタリング分析
- car: 統計分析補助
- Cairo: 高品質なグラフィックス出力
- svglite: SVG形式での出力

パッケージのインストールは以下のコマンドで行えます：
```R
install.packages(c("tidyverse", "ggbeeswarm", "e1071", "scales", "cluster", "car", "Cairo", "svglite"))
```

## アンインストール方法

1. **R環境からのアンインストール**
```r
remove.packages("jaciii2025")
```

2. **GitHubからクローンした場合**
- プロジェクトディレクトリを削除してください
```bash
rm -rf jaciii2025
```

## 注意事項
- 分析結果は `sample_output` ディレクトリに自動的に保存されます
- 言語設定は `build_sample.R` の `lang` パラメータで変更可能です（"ja"または"en"）
