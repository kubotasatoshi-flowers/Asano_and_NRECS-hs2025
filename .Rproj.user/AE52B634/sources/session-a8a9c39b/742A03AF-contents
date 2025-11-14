library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(readr)
library(ggpp)

#コメント
comment_text <- paste("Bubbling:Off, Well_Sv:28°C/27°C")
0
#データ表示範囲指定
from <- as.POSIXct("2025-08-25 06:00:00", tz = tz_use) 
to   <- as.POSIXct("2025-08-27 06:00:00", tz = tz_use)

#PDFファイル名
fn <- c("Temp_water_HP_0825_0827")








#ひな形を読み込み
source("source/Theme_Box_line.R")

#タイムゾーンの指定
tz_use <- "Asia/Tokyo"

#csvからデータ読み込み
data <- read.table("/cloud/project/data/Water_HP_in_out_1st_2nd_WT.csv",sep=",", comment.char="#", header=T) 

#列1の名前を変更
names(data)[1] <- "datetime"

#列１をPOSIXの日時データに変換
data[,1] <- as.POSIXct(data[,1], format = "%Y-%m-%d %H:%M:%S", tz="Asia/Tokyo")



# データの整形# データをlong形式に変換
df_long <-
  data |> pivot_longer(
    cols = c("X1st_out_WT","X1st_in_WT","X2nd_out_WT","X2nd_in_WT"),
    names_to = "Type",
    values_to = "Value"
    )

#項目の名称変更
df_long <-  df_long %>%
  mutate(Type = recode(Type,
                       "X1st_out_WT" = "Out ",
                       "X1st_in_WT" = "In ",
                       "X2nd_out_WT" = "Out",
                       "X2nd_in_WT" = "In"
                       )
         )
  
#データ表示範囲にデータを整形
df_long <-  df_long %>%
  filter(datetime >= from, datetime <= to) |>
  filter(!is.na(Value))


# df_long <- df_long %>%
#   mutate(
#     datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = tz_use)
#   )

#図を上下2段に重ねる設定
# 上段の系列名と、下段の系列名の指定
top_pair    <- c("Out ", "In ")
bottom_pair <- c("Out", "In")

# 数値保証＆パネル割当
df_plot <- df_long %>%
  mutate(
    Value = suppressWarnings(parse_number(as.character(Value))),
    Type = as.character(Type),
    panel  = case_when(Type %in% top_pair    ~ paste(top_pair,    collapse = " & "),
                       Type %in% bottom_pair ~ paste(bottom_pair, collapse = " & "),
                       TRUE ~ NA_character_)
  ) %>%
  filter(!is.na(panel)) %>%
  mutate(
    # 表示順（上段→下段）
    panel  = factor(panel,  levels = c(paste(top_pair, collapse=" & "),
                                       paste(bottom_pair, collapse=" & "))),
    # 凡例の系列順（任意）
    Type = factor(Type, levels = c(top_pair, bottom_pair))
  )

# 同じ日の2本目以降は日付を空欄にするラベラー
lab_date_time_once <- function(x, tz = tz_use) {
  d <- as.Date(x, tz = tz)
  first <- c(TRUE, d[-1] != d[-length(d)])
  paste0(ifelse(first, format(x, "%m-%d", tz = tz), ""),
         "\n",
         format(x, "%k:%M", tz = tz) |> trimws()) #06:00を6:00にするための処理
}

breaks_h <- seq(from, to, by = "12 hours")  # POSIXct


# 色テーブル（既存の指定を流用）
col_map <- c(
  "Out "   = "darkblue",
  "In "    = "red",
  "Out" = "darkgreen",
  "In"  = "orange"
)


# --- 上段（Primary in/out）だけのプロット -------------------------------
p_top <- ggplot(
  df_plot |> dplyr::filter(Type %in% top_pair),
  aes(x = datetime, y = Value, color = Type, group = Type)
) +
  geom_line(stat = "identity", position = "identity", size = 0.6) +
      scale_color_manual(
      values = col_map[top_pair],  # 使う色だけ
      breaks = top_pair,           # 凡例の順序と表示対象を制御
      limits = top_pair,
      name   = NULL
    ) +
    scale_x_datetime(
      limits = c(from, to),
      breaks = breaks_h,
      minor_breaks = NULL,
      labels = lab_date_time_once,
      name = NULL
    ) +
    scale_y_continuous(
      limits = c(5, 40),
      breaks = seq(5, 40, 5),
      minor_breaks = NULL,
      name = "Temperature (°C)"
    ) +
  ggtitle("Water HP:Primary") +
  theme_minimal() +
  Theme_Box_line(base_family = "latin-times") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(legend.direction = "horizontal",legend.position = c(1, 1))


# --- 下段（Secondary in/out）だけのプロット -----------------------------
p_bottom <- ggplot(
  df_plot |> dplyr::filter(Type %in% bottom_pair),
  aes(x = datetime, y = Value, color = Type, group = Type)
) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  scale_color_manual(
    values = col_map[bottom_pair],
    breaks = bottom_pair,
    limits = bottom_pair,
    name   = NULL
  ) +
  scale_x_datetime(
    limits = c(from, to),
    breaks = breaks_h,
    minor_breaks = NULL,
    labels = lab_date_time_once,
    name = "2025"
  ) +
  scale_y_continuous(
    limits = c(5, 40),
    breaks = seq(5, 40, 5),
    minor_breaks = NULL,
    name = "Temperature (°C)"
  ) +
  ggtitle("Water HP:Secondary") +
  theme_minimal() +
  Theme_Box_line(base_family = "latin-times") +
  theme(legend.direction = "horizontal",legend.position = c(1, 1))


label_plot <- ggplot() +
  theme_void() +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  annotate(
    "text",
    x = 0.02, y = 0.87,                # ← 左に少しパディング，テキスト枠の位置調整
    label = comment_text,
    hjust = 0, vjust = 0.5,           # ← 左揃え
    family = "latin-times",
    size = 12 / ggplot2::.pt,         # ← 12pt 相当（pt→ggplot換算）
    lineheight = 1.1
  )

pp_top <- p_top + inset_element(label_plot, #テキストを入れる小窓を作成する
                                left=0, #左端の位置
                                right=1, #パネルの幅の100%まで
                                bottom=0.35,#パネルの高さ35%から
                                top=1,#パネルの高さ100%まで
                                align_to = "panel")

# --- 上下に結合。guides="keep" で各プロットの凡例を保持 -----------------

(pp_top / p_bottom) + plot_layout(guides = "keep")

ggsave(paste0("/cloud/project/figs/",fn,".pdf"),
       plot = (pp_top / p_bottom) + plot_layout(guides = "keep"),
       device=cairo_pdf,
       width = 15,
       height = 18,
       units = "cm")
