if (interactive() && requireNamespace("renv", quietly = TRUE) && file.exists("renv.lock")) {
  lib_empty <- !dir.exists(file.path("renv","library")) || length(list.files(file.path("renv","library"), recursive = TRUE)) == 0
  if (lib_empty && !nzchar(Sys.getenv("NO_AUTO_RENV_RESTORE"))) {
    message("[.Rprofile] 初回セットアップ：renv::restore() を自動実行します（NO_AUTO_RENV_RESTORE=1 で無効化）。")
    renv::restore(prompt = FALSE)
  }
}

source("renv/activate.R")
# ===========================
# Posit Cloud / RStudio
# 軽量起動・再現性（renv）・フォント設定
# ===========================

# タイムゾーンと識別情報
Sys.setenv(TZ = "Asia/Tokyo")
options(.rprofile_loaded_time = Sys.time(), .rprofile_tag = "project")

# 端末操作または knitting 中のみ実行
if (interactive() || isTRUE(getOption("knitr.in.progress"))) {
  
  # --- 軽量モード（環境変数で全スキップ） ---
  if (nzchar(Sys.getenv("LIGHT_STARTUP"))) {
    message("[.Rprofile] LIGHT_STARTUP=1: heavy init をスキップします。")
  } else {
    
    # --- renv: 再現性の案内（自動インストールはしない） ---
    if (requireNamespace("renv", quietly = TRUE) && file.exists("renv.lock")) {
      # まだ復元していないっぽい時だけ案内（簡易判定）
      restored <- dir.exists(file.path("renv", "library")) || nzchar(Sys.getenv("RENV_PATHS_LIBRARY"))
      if (!restored) {
        packageStartupMessage(
          "[.Rprofile] 再現性のため、最初に以下を実行してください：\n",
          "  renv::restore()\n"
        )
      }
    }
    
    # --- showtext/sysfonts/ggplot2: フォント＋テーマ（遅延適用） ---
    if (requireNamespace("showtext", quietly = TRUE) &&
        requireNamespace("sysfonts", quietly = TRUE) &&
        requireNamespace("ggplot2", quietly = TRUE)) {
      
      # 1) まずはローカル同梱フォントを優先
      #    （プロジェクトに assets/fonts/ を置く場合）
      fontdir <- file.path(getwd(), "assets", "fonts")
      have_local <- dir.exists(fontdir)
      
      fams <- sysfonts::font_families()
      
      add_if_missing <- function(family, add_fun) {
        if (!(family %in% sysfonts::font_families())) add_fun()
      }
      
      if (have_local) {
        add_if_missing("latin-times", function() {
          sysfonts::font_add(
            family = "latin-times",
            regular    = file.path(fontdir, "Tinos-Regular.ttf"),
            bold       = file.path(fontdir, "Tinos-Bold.ttf"),
            italic     = file.path(fontdir, "Tinos-Italic.ttf"),
            bolditalic = file.path(fontdir, "Tinos-BoldItalic.ttf")
          )
        })
        add_if_missing("jp-serif", function() {
          # Noto Serif JP は OTF/TTF どちらでも可。ファイル名は適宜合わせてください。
          cand <- c("NotoSerifJP-Regular.otf","NotoSerifJP-Regular.ttf")
          path <- cand[file.exists(file.path(fontdir, cand))][1]
          if (!is.na(path)) {
            sysfonts::font_add(family = "jp-serif", regular = file.path(fontdir, path))
          }
        })
      } else {
        # 2) ローカルが無い場合のみ Google から（初回のみDLでやや重い）
        add_if_missing("latin-times", function() {
          sysfonts::font_add_google("Tinos", "latin-times")
        })
        add_if_missing("jp-serif", function() {
          sysfonts::font_add_google("Noto Serif JP", "jp-serif")
        })
      }
      
      # 必要時のみフォント描画を有効化（編集中/ニット中）
      showtext::showtext_auto(enable = TRUE)
      # 図の文字のにじみを安定化したい場合は DPI を固定（必要に応じて）
      # showtext::showtext_opts(dpi = 300)
      
      # ggplot2 がロードされた「後」にテーマを設定（起動直後は何もしない）
      setHook(
        packageEvent("ggplot2", "onLoad"),
        function(...) {
          ggplot2::theme_set(ggplot2::theme_minimal(base_family = "latin-times"))
          ggplot2::update_geom_defaults("text",  list(family = "latin-times"))
          ggplot2::update_geom_defaults("label", list(family = "latin-times"))
          packageStartupMessage("[.Rprofile] ggplot2 theme = theme_minimal('latin-times')")
        }
      )
    } else {
      message("[.Rprofile] showtext/sysfonts/ggplot2 が見つかりません。フォント/テーマ設定をスキップします。")
    }
  }
}
