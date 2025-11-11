library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(readr)
library(ggpp)
library(hms)

source("source/Theme_Box_line.R")

#タイムゾーンの指定
tz_use <- "Asia/Tokyo"

#データ表示範囲指定
from <- as.POSIXct("2025-08-01", tz = tz_use) 
to   <- as.POSIXct("2025-08-31", tz = tz_use)


#csvからデータ読み込み
data <- read.table("/cloud/project/data/Well_time.csv",sep=",", comment.char="#", header=T) 

#必要項目の抽出
data_select <- select(data,2:4)

#列dayをPOSIXの日時データに変換
data_select$day <- as.POSIXct(data[,2], format = "%Y-%m-%d", tz="Asia/Tokyo")

#列off_secondsをPOSIXの時間データに変換
data_select$off_time_hms <- as_hms(data_select$off_time_hms)

#稼働時間から井戸水の使用量を計算し，計算結果の列を追加
data_select <-
  data_select %>%
    mutate(
      water_kL = round(off_seconds/60*10/1000,2) #稼働時間（s）を分に変換し，10L/minを乗じてkLとし，小数点2桁まで表示
           )

#ロング形式に変換
df_long <- 
  data_select %>%
  pivot_longer(
    cols=(c("off_seconds","water_kL")),
    names_to="Items",
    values_to="V"
    )

#データ表示範囲にデータを整形
df_long <-  df_long %>%
  filter(day >= from, day <= to) |>
  filter(!is.na(V)
  )

#グラフに表示する項目を指定
select <- c("water_kL")

# 項目の色を指定
col_map <- c("water_kL" = "red")

# 同じ日の2本目以降は日付を空欄にするラベラー
lab_date_time_once <- function(x, tz = tz_use) {
  d <- as.Date(x, tz = tz)
  first <- c(TRUE, d[-1] != d[-length(d)])
  paste0(ifelse(first, format(x, "%m-%d", tz = tz), "")
         #,\n",
         #format(x, "%k:%M", tz = tz) |> trimws()
         ) #06:00を6:00にするための処理
}


p <- ggplot(
  df_long |> dplyr::filter(Items %in% select),
  aes(x = day, y = V, color = Items, group = Items)
)+
  geom_line(stat = "identity", position = "identity", size = 0.6) +
  geom_point(size = 3)+
  guides(color = guide_legend(nrow = 1, ncol = 1)) + 
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
    limits = c(0, 20),
    breaks = seq(0, 20, 5),
    minor_breaks = seq(0,20,1),
    name = "Daily water supply (kL/day)"
  ) +
  ggtitle("Daily water supply into primary heat exchange tank") +
  theme_minimal() +
  Theme_Box_line(base_family = "latin-times") +
  theme(
    #凡例の書式などの指定
    legend.position = "none",
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(linewidth = 0.1, color="black"),
    panel.grid.major.y = element_line(linewidth = 0.1, color="black"),
    #タイトルの位置などの指定
    plot.title = element_text(hjust = 0.5)
  )
p

ggsave("/cloud/project/figs/daily_water_supply_of_primary_HEtank.pdf",
       plot = p,
       device=cairo_pdf,
       width = 10,
       height = 10,
       units = "cm")

