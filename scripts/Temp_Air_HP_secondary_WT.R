library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(readr)
library(ggpp)

#ひな形を読み込み
source("source/Theme_Box_line.R")

#タイムゾーンの指定
tz_use <- "Asia/Tokyo"

#csvからデータ読み込み
data <- read.table("/cloud/project/data/Air_HP_in_out_2nd_WT.csv",sep=",", comment.char="#", header=T) 

#列1の名前を変更
names(data)[1] <- "datetime"

#列１をPOSIXの日時データに変換
data[,1] <- as.POSIXct(data[,1], format = "%Y-%m-%d %H:%M", tz="Asia/Tokyo")

#データ表示範囲指定
from <- as.POSIXct("2025-08-18 00:00:00", tz = tz_use) 
to   <- as.POSIXct("2025-08-31 23:59:00", tz = tz_use)

# データの整形# データをlong形式に変換
df_long <-
  data |> pivot_longer(
    cols = c("Snd_out_WT","Snd_in_WT"),
    names_to = "Type",
    values_to = "Value",
    values_transform = list(Value = parse_number)
    )

  
#データ表示範囲にデータを整形
df_long <-  df_long %>%
  filter(datetime >= from, datetime <= to) |>
  filter(!is.na(Value))

#項目の名称変更
df_long <-  df_long %>%
  mutate(Type = recode(Type,
                       "Snd_out_WT" = "Outlet",
                       "Snd_in_WT" = "Inlet"
  ))

# 同じ日の2本目以降は日付を空欄にするラベラー
lab_date_time_once <- function(x, tz = tz_use) {
  d <- as.Date(x, tz = tz)
  first <- c(TRUE, d[-1] != d[-length(d)])
  paste0(ifelse(first, format(x, "%m-%d", tz = tz), ""))
         # ,
         # "\n",
         # format(x, "%k:%M", tz = tz) |> trimws()) #06:00を6:00にするための処理
}

breaks_h <- seq(from, to, by = "12 hours")  # POSIXct

select <- c("Inlet","Outlet")

# 色テーブル（既存の指定を流用）
col_map <- c("Inlet"   = "darkblue","Outlet"    = "red")

p <- ggplot(
  df_long |> dplyr::filter(Type %in% select),
  aes(x = datetime, y = Value, color = Type, group = Type)
)+
  geom_line(stat = "identity", position = "identity", size = 0.3) +
      scale_color_manual(
      values = col_map[select],  # 使う色だけ
      breaks = select,           # 凡例の順序と表示対象を制御
      limits = select,
      name   = NULL
    ) +
    scale_x_datetime(
      limits = c(from, to),
      breaks = seq(from, to, by = "5 days"),
      minor_breaks = seq(from, to, by = "1 days"),
      labels = lab_date_time_once,
      name = "2025"
    ) +
    scale_y_continuous(
      limits = c(0, 30),
      breaks = seq(0, 30, 5),
      minor_breaks = NULL,
      name = "Water temperature (°C)"
    ) +
  ggtitle("Secondary at Air HP") +
  theme_minimal() +
  Theme_Box_line(base_family = "latin-times") +
  theme(
    legend.direction = "horizontal",
    legend.position = c(0.95, 0.98),
    legend.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(size = 0.1, color="black"),
    panel.grid.major.y = element_line(size = 0.1, color="black")
    )

p

ggsave("/cloud/project/figs/Temp_Air_HP_WT.pdf",
       plot = p,
       device=cairo_pdf,
       width = 10,
       height = 10,
       units = "cm")
