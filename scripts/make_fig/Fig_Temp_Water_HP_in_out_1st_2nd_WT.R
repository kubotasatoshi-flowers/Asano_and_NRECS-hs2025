#地下水熱源式HPの1次側と2次側の水温と貯水タンクの温度をプロットする

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(readr)
library(ggpp)
library(forcats)
library(hms)
library(lubridate)

#ひな形を読み込み
source("source/Theme_Box_line.R")

Bubbling <-"Off"
Start_temp <- "22°C"
End_temp <-"21°C"

comment_text <- paste("Bubbling:",Bubbling, ",","Well_temp_L:",End_temp,",","Well_temp_U:",Start_temp)



#タイムゾーンの指定
tz_use <- "Asia/Tokyo"

# RDS から温度データを読み込み（地下水熱源式HPの水温）
WT_rds <- readRDS("/cloud/project/data/rds/TEMP_Water_HP_1st_2nd_Well_WT_20250811_1023.rds")
WT_wide <- WT_rds$data #データ部分のみを取り出す
WT_wide <- WT_wide %>%
  rename("1st out"="Primary_out_WT",
         "1st in"="Primary_in_WT",
         "2nd out"="Secondary_out_WT",
         "2nd in"="Secondary_in_WT",
         "Tank"="Well_Tank_T")

#データ表示範囲指定
from <- as.POSIXct("2025-08-11 00:00:00", tz = tz_use) 
to   <- as.POSIXct("2025-09-10 23:59:00", tz = tz_use)

data_join <- WT_wide %>%
  mutate(Date = as_date(Time_stamp, tz = "Asia/Tokyo")) %>%
  mutate (Hours = as_hms(Time_stamp))%>%
  filter (Hours >= as_hms("0:00:00") & Hours <= as_hms("23:59:00"))

# データの整形# データをlong形式に変換
df_long <-
  data_join %>% pivot_longer(
    cols = -c(Time_stamp,Date, Hours),
    #cols = -c(Time_stamp),
    names_to = "Type",
    values_to = "Value"
    )

  
#データ表示範囲にデータを整形
df_long <-  df_long %>%
  filter(Time_stamp >= from, Time_stamp <= to) %>%
  filter(!is.na(Value))

#図を上下2段に重ねる設定
# 上段の系列名と、下段の系列名の指定
top_pair    <- c("1st out", "1st in", "Tank")
bottom_pair <- c("2nd out", "2nd in")


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
         "\n")
  #,
   #      format(x, "%k:%M", tz = tz) |> trimws()) #06:00を6:00にするための処理
}

breaks_h <- seq(from, to, by = "5 days")  # POSIXct

# 色テーブル（既存の指定を流用）
col_map <- c(
  "1st in"   = "darkblue",
  "1st out"    = "red",
  "Tank" = "darkgreen",
  "2nd in"  = "red",
  "2nd out"  = "darkblue"
)


# --- 上段（Primary in/out）だけのプロット -------------------------------
p_top <- ggplot(
  df_plot %>% filter(Type %in% top_pair),
  aes(x = Time_stamp,
      y = Value,
      color = Type,
      group = interaction(Date, Type, drop = TRUE))
) +
  geom_line(stat = "identity", position = "identity", size = 0.3) +
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
    date_labels = "%b-%d",
    labels = lab_date_time_once,
    name = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 45),
    breaks = seq(0, 45, 5),
    minor_breaks = NULL,
    name = "Temperature (°C)"
  ) +
  ggtitle("Primary and Tank") +
  theme_minimal() +
  Theme_Box_line(base_family = "latin-times") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(legend.direction = "horizontal",legend.position = c(1, 1))

# --- 下段（Secondary in/out）だけのプロット -----------------------------
p_bottom <- ggplot(
  df_plot %>% filter(Type %in% bottom_pair),
  aes(x = Time_stamp,
      y = Value,
      color = Type,
      group = interaction(Date, Type, drop = TRUE))
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
    limits = c(0, 40),
    breaks = seq(0, 40, 5),
    minor_breaks = NULL,
    name = "Temperature (°C)"
  ) +
  ggtitle("Secondary") +
  theme_minimal() +
  Theme_Box_line(base_family = "latin-times") +
  theme(legend.direction = "horizontal",legend.position = c(0.7, 1))

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


#p_top / p_bottom

# 
# 
# 
# ggsave("/cloud/project/figs/?????????????.pdf",
#        plot = p,
#        device=cairo_pdf,
#        width = 15,
#        height = 10,
#        units = "cm")
