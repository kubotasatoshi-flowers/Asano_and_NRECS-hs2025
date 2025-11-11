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
data <- read.table("/cloud/project/data/TEMP_RZ_A_from_20250801.csv",sep=",", comment.char="#", header=T) 

data$RZT_NRECS_W_HP <- as.numeric(data$RZT_NRECS_W_HP)
data$RZT_NRECS_A_HP <- as.numeric(data$RZT_NRECS_A_HP)
#列1の名前を変更
names(data)[1] <- "datetime"

#列１をPOSIXの日時データに変換
data[,1] <- as.POSIXct(data[,1], format = "%Y-%m-%d %H:%M:%S", tz="Asia/Tokyo")

#データ表示範囲指定
from <- as.POSIXct("2025-08-01 00:00:00", tz = tz_use) 
to   <- as.POSIXct("2025-08-31 23:59:00", tz = tz_use)

# データの整形# データをlong形式に変換
df_long <-
  data |> pivot_longer(
    cols = c("Air_temp","RZT_Cont","RZT_NRECS_W_HP","RZT_NRECS_A_HP"),
    names_to = "Type",
    values_to = "Value"
  )

#項目の名称変更
df_long <-  df_long %>%
  mutate(Type = recode(Type,
                       "Air_temp" = "Air temp",
                       "RZT_Cont" = "RZT at control",
                       "RZT_NRECS_W_HP" = "RZT at N.RECS with W-HP",
                       "RZT_NRECS_A_HP" = "RZT at N.RECS with A-HP"))

#データ表示範囲にデータを整形
df_long <-  df_long %>%
  filter(datetime >= from, datetime <= to) |>
  filter(!is.na(Value))

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

select <- c("Air temp","RZT at control","RZT at N.RECS with W-HP","RZT at N.RECS with A-HP")

# 色テーブル（既存の指定を流用）
col_map <- c("Air temp" = "black",
             "RZT at control" = "red",
             "RZT at N.RECS with W-HP"="blue",
             "RZT at N.RECS with A-HP"="green")

p <- ggplot(
  df_long |> dplyr::filter(Type %in% select),
  aes(x = datetime, y = Value, color = Type, group = Type)
)+
  geom_line(stat = "identity", position = "identity", size = 0.3) +
  guides(color = guide_legend(nrow = 2, ncol = 2)) + 
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
    limits = c(0, 45),
    breaks = seq(0, 45, 5),
    minor_breaks = NULL,
    name = "Temperature (°C)"
  ) +
  ggtitle("Air temperature and root-zone temperature") +
  theme_minimal() +
  Theme_Box_line(base_family = "latin-times") +
  theme(
    legend.direction = "horizontal",
    legend.position = c(0.95, 0.3),
    legend.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(size = 0.1, color="black"),
    panel.grid.major.y = element_line(size = 0.1, color="black")
  )

p

ggsave("/cloud/project/figs/Temp_Air_RZT_cont_NRECS.pdf",
       plot = p,
       device=cairo_pdf,
       width = 15,
       height = 10,
       units = "cm")