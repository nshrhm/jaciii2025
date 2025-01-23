# utils.R

#' @title 環境設定の初期化
#' @description システムの環境設定、ロケール、グラフィックデバイスの初期化を行う
#' @param verbose デバッグ情報の表示フラグ
#' @param install_packages 不足パッケージの自動インストールフラグ
#' @param force_cairo Cairoデバイスの強制使用フラグ
#' @return list 設定結果を含むリスト
#'   \item{success}{bool 全体の成功状態}
#'   \item{locale}{character 設定されたロケール}
#'   \item{graphics_device}{character 設定されたグラフィックデバイス}
#'   \item{messages}{character vector 処理中のメッセージ}
initialize_environment <- function(verbose = FALSE,
                                   install_packages = TRUE,
                                   force_cairo = FALSE) {
  messages <- character(0)
  add_message <- function(msg) {
    messages <<- c(messages, msg)
    if (verbose) message(msg)
  }

  # 結果の初期化
  result <- list(
    success = TRUE,
    locale = NULL,
    graphics_device = NULL,
    messages = NULL
  )

  # 現在のロケールを確認
  current_locale <- Sys.getlocale()
  add_message(sprintf("現在のロケール設定: %s", current_locale))

  # ロケールの設定
  tryCatch(
    {
      if (.Platform$OS.type == "windows") {
        locale_options <- c("Japanese", "Japanese_Japan.UTF-8")
      } else {
        locale_options <- c("ja_JP.UTF-8", "ja_JP.utf8")
      }

      locale_set <- FALSE
      for (locale in locale_options) {
        tryCatch(
          {
            Sys.setlocale("LC_ALL", locale)
            add_message(sprintf("ロケールを %s に設定しました", locale))
            locale_set <- TRUE
            result$locale <- locale
            break
          },
          error = function(e) {
            add_message(sprintf("%s ロケールの設定に失敗しました", locale))
          }
        )
      }

      if (!locale_set) {
        result$success <- FALSE
        add_message("利用可能な日本語ロケールの設定に失敗しました")
      }
    },
    error = function(e) {
      result$success <- FALSE
      add_message(sprintf("ロケール設定中にエラーが発生: %s", e$message))
    }
  )

  # グラフィックデバイスの設定
  if (force_cairo || capabilities("cairo")) {
    # Cairoパッケージの確認とインストール
    if (!requireNamespace("Cairo", quietly = TRUE)) {
      if (install_packages) {
        add_message("Cairoパッケージをインストールします")
        tryCatch(
          {
            install.packages("Cairo")
            library(Cairo)
            add_message("Cairoパッケージのインストールと読み込みに成功しました")
          },
          error = function(e) {
            result$success <- FALSE
            add_message(sprintf("Cairoパッケージのインストールに失敗: %s", e$message))
            return()
          }
        )
      } else {
        add_message("Cairoパッケージが必要です")
        result$success <- FALSE
        return()
      }
    }

    # グラフィックデバイスの設定
    options(device = "cairo")
    options(bitmapType = "cairo")
    result$graphics_device <- "cairo"
    add_message("Cairoグラフィックデバイスを設定しました")
  } else {
    result$graphics_device <- getOption("device")
    add_message("デフォルトのグラフィックデバイスを使用します")
  }

  result$messages <- messages
  return(result)
}

#' @title データの読み込みと前処理の改善版
#' @param file_path データファイルのパス
#' @param lang 言語コード
#' @return データフレームのリスト（raw と long 形式）
load_and_preprocess_data <- function(file_path, lang = "ja") {
  # データの読み込み
  tryCatch(
    {
      # readr::read_csv を使用してデータを読み込む
      data <- read_csv(file_path,
        col_types = cols(
          ID = col_character(),
          Q1 = col_double(),
          Q2 = col_double(),
          Q3 = col_double(),
          Q4 = col_double(),
          Q5 = col_double(),
          Country = col_character()
        )
      )

      # データの検証
      if (nrow(data) == 0) {
        stop("Empty dataset")
      }

      required_cols <- c("ID", paste0("Q", 1:5), "Country")
      missing_cols <- setdiff(required_cols, names(data))
      if (length(missing_cols) > 0) {
        stop(sprintf(
          "Missing required columns: %s",
          paste(missing_cols, collapse = ", ")
        ))
      }

      # 長形式データの作成
      data_long <- data %>%
        pivot_longer(
          cols = starts_with("Q"),
          names_to = "question",
          values_to = "value"
        ) %>%
        mutate(
          question = factor(
            question,
            levels = paste0("Q", 1:5),
            labels = sapply(paste0("Q", 1:5), function(q) {
              get_translation(q, "questions", lang)
            })
          )
        )

      return(list(
        raw = data,
        long = data_long
      ))
    },
    error = function(e) {
      message(sprintf("Error in data loading: %s", e$message))
      return(NULL)
    }
  )
}

#' @title 結果の保存
#' @description データフレームやプロットを各種形式で保存する統合関数
#' @param data 保存するデータ (データフレームまたはggplotオブジェクト)
#' @param file_path ファイルパス
#' @param type オプションで指定する保存形式 ("csv", "pdf", "svg")。指定しない場合は拡張子から判断
#' @return invisible(TRUE) if successful, invisible(FALSE) if failed
#' @examples
#' # データフレームをCSVとして保存
#' df <- data.frame(x = 1:3, y = letters[1:3])
#' save_results(df, "output/data.csv")
#'
#' # ggplotをPDFとして保存
#' p <- ggplot(df, aes(x, y)) +
#'   geom_point()
#' save_results(p, "output/plot.pdf")
save_results <- function(data, file_path, type = NULL) {
  # ディレクトリの作成
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)

  # ファイル形式の判定
  if (is.null(type)) {
    type <- tolower(tools::file_ext(file_path))
  }

  tryCatch(
    {
      if (type == "csv") {
        # CSVファイルの保存
        if (!is.data.frame(data)) {
          stop("CSVファイルの保存にはデータフレームが必要です")
        }
        write_csv(data, file_path)
      } else if (type %in% c("pdf", "svg")) {
        # プロットの保存
        if (!inherits(data, "ggplot")) {
          stop("PDF/SVGファイルの保存にはggplotオブジェクトが必要です")
        }

        # デバイスの設定
        device <- if (type == "pdf") cairo_pdf else "svg"

        # プロットの保存
        ggsave(
          filename = file_path,
          plot = data,
          device = device,
          width = 10,
          height = 7,
          units = "in"
        )
      } else {
        stop(sprintf("未対応のファイル形式です: %s", type))
      }

      invisible(TRUE)
    },
    error = function(e) {
      warning(sprintf("ファイル保存でエラーが発生: %s", e$message))
      invisible(FALSE)
    }
  )
}

#' @title プロットの共通テーマを設定する関数
#' @description ggplotオブジェクトに対して、統一されたテーマ設定を適用します
#' @param p ggplotオブジェクト
#' @param base_size ベースとなるフォントサイズ（デフォルト：14）
#' @param rotate_x_labels X軸ラベルを45度回転するかどうか（デフォルト：FALSE）
#' @return テーマが適用されたggplotオブジェクト
#' @export
apply_common_theme <- function(base_size = 14, rotate_x_labels = FALSE) {
  common_theme <- theme(
    text = element_text(family = "sans"),
    plot.title = element_text(size = rel(1.2), hjust = 0.5),
    axis.title = element_text(size = rel(1.1)),
    axis.text = element_text(size = rel(1.0)),
    axis.text.x = if (rotate_x_labels) element_text(angle = 45, hjust = 1) else element_text(),
    legend.title = element_text(size = rel(1.0)),
    legend.text = element_text(size = rel(0.9)),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    legend.position = "right"
  )
  theme_minimal(base_size = base_size) + common_theme
}
