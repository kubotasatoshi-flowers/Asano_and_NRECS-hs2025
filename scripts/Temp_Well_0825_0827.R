library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(readr)
library(ggpp)

source("source/Theme_Box_line.R")

#タイムゾーンの指定
tz_use <- "Asia/Tokyo"

#csvからデータ読み込み
data <- read.table("/cloud/project/data/TEMP_NRECS_from_20250801.csv",sep=",", comment.char="#", header=T) 

#列1の名前を変更
names(data)[1] <- "datetime"

#列１をPOSIXの日時データに変換
data[,1] <- as.POSIXct(data[,1], format = "%Y-%m-%d %H:%M:%S", tz="Asia/Tokyo")

#データ表示範囲指定
from <- as.POSIXct("2025-08-25 06:00:00", tz = tz_use) 
to   <- as.POSIXct("2025-08-27 06:00:00", tz = tz_use)

# データの整形# データをlong形式に変換
df_long <-
  data |> pivot_longer(
    cols = c("Soil_T1","Soil_T2","Soil_T3","Soil_T4","Soil_T5","Soil_T6","Well_Tank_T","Well_Water_T"),
    names_to = "Type",
    values_to = "Value"
  )

#項目の名称変更
df_long <-  df_long %>%
  mutate(Type = recode(Type,
                       "Soil_T1" = "RZT at N.RECS",
                       "Well_Tank_T" = "Well tank",
                       "Well_Water_T" = "Well water"))

#データ表示範囲にデータを整形
df_long <-  df_long %>%
  filter(datetime >= from, datetime <= to) |>
  filter(!is.na(Value))

# 同じ日の2本目以降は日付を空欄にするラベラー
lab_date_time_once <- function(x, tz = tz_use) {
  d <- as.Date(x, tz = tz)
  first <- c(TRUE, d[-1] != d[-length(d)])
  paste0(ifelse(first, format(x, "%m-%d", tz = tz), ""),
   "\n",
  format(x, "%k:%M", tz = tz) |> trimws()) #06:00を6:00にするための処理
}

breaks_h <- seq(from, to, by = "12 hours")  # POSIXct

select <- c("Well tank","Well water")

# 色テーブル（既存の指定を流用）
col_map <- c("Well tank"   = "darkblue","Well water"    = "red")

p <- ggplot(
  df_long |> dplyr::filter(Type %in% select),
  aes(x = datetime, y = Value, color = Type, group = Type)
)+
  geom_line(stat = "identity", position = "identity", size = 0.6) +
  scale_color_manual(
    values = col_map[select],  # 使う色だけ
    breaks = select,           # 凡例の順序と表示対象を制御
    limits = select,
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
    breaks = seq(5, 40, 5),
    minor_breaks = NULL,
    name = "Temperature (°C)"
  ) +
  ggtitle("Bubbling:Off, Well_Sv:28°C/27°C") +
  theme_minimal() +
  Theme_Box_line(base_family = "latin-times") +
  theme(
    legend.direction = "horizontal",
    legend.position = c(0.95, 0.2),
    legend.background = element_rect(fill = "white"),
    #panel.grid.major.x = element_line(size = 0.1, color="black"),
    #panel.grid.major.y = element_line(size = 0.1, color="black")
  )

p

ggsave("/cloud/project/figs/Temp_well_0825_0827.pdf",
       plot = p,
       device=cairo_pdf,
       width = 15,
       height = 9,
       units = "cm")