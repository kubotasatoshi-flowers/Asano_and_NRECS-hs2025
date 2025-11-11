#修正した気温データを使って最高，平均，最低気温の推移を作図する
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(readr)
library(ggpp)
library(pracma)
library(lubridate)

source("source/Theme_Box_line.R")

#タイムゾーンの指定
tz_use <- "Asia/Tokyo"


# RDS から温度データを読み込み
data_rds <- readRDS("/cloud/project/data/rds/TEMP_Hampel_AirHP_2nd_WT.rds")
data <- data_rds$data #データ部分のみを取り出す


#データ表示範囲指定
from <- as.POSIXct("2025-09-20 00:00:00", tz = tz_use) 
to   <- as.POSIXct("2025-10-10 23:59:00", tz = tz_use)

# データの整形# データをlong形式に変換
df_long <-
  data %>%
  filter (Time_stamp >= from, Time_stamp <= to) %>%
  filter (Type %in% "Snd_out_WT")

df_plot <- df_long %>%
  pivot_longer (cols = -c("Time_stamp","Type","is_outlier"),
                names_to = "T",
                values_to = "V") %>%
  filter (T %in% "Org_V")



#作図するデータ系列を指定1
select <- c("Org_V")

# 色テーブル
col_map <- c("Org_V" ="darkblue")

p <- ggplot(df_plot,
  aes(x = Time_stamp, y = V, color = T, group = T)
)+

  geom_line(stat = "identity", position = "identity", size = 0.3) +
  #geom_point(stat = "identity", position = "identity", size = 0.5) +
  scale_color_manual(
     values = col_map,  # 使う色だけ
     breaks = select,           # 凡例の順序と表示対象を制御
     limits = select,
     name   = NULL
   ) +
  scale_x_datetime(
    limits = c(from, to),
    breaks = seq(from, to, by = "2 days"),
    minor_breaks = seq(from, to, by = "1 days"),
    date_labels = "%b-\n%d",
    name = "2025",
  ) +

  scale_y_continuous(
    limits = c(0, 40),
    breaks = seq(0, 40, 5),
    minor_breaks = seq(0, 40, 1),
    name = "Air temperature (°C)"
  ) +
  
  labs(
    title =NULL,
    subtitle =NULL,
    caption =NULL,
      #"Changes of air temperature in greehouse during the experiment."
    )+

  theme_minimal() +
  Theme_Box_line(base_family = "latin-times") +
  theme(
    legend.direction = "horizontal",
    #legend.position = c(0.8, 0.4),
    legend.position = "top",
    legend.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(size = 0.1, color="black"),
    panel.grid.major.y = element_line(size = 0.1, color="black"),

  )

#作図するデータ系列を指定2
df_plot <- df_long %>%
  pivot_longer (cols = -c("Time_stamp","Type","is_outlier"),
                names_to = "T",
                values_to = "V") %>%
  filter (T %in% "Hampel_V")

select <- c("Hampel_V")

# 色テーブル
col_map <- c("Hampel_V" ="red" )




p1 <- ggplot(df_plot,
            aes(x = Time_stamp, y = V, color = T, group = T)
)+
  
  geom_line(stat = "identity", position = "identity", size = 0.3) +
  #geom_point(stat = "identity", position = "identity", size = 0.5) +
  scale_color_manual(
    values = col_map,  # 使う色だけ
    breaks = select,           # 凡例の順序と表示対象を制御
    limits = select,
    name   = NULL
  ) +
  scale_x_datetime(
    limits = c(from, to),
    breaks = seq(from, to, by = "2 days"),
    minor_breaks = seq(from, to, by = "1 days"),
    date_labels = "%b-\n%d",
    name = "2025",
  ) +
  
  scale_y_continuous(
    limits = c(0, 40),
    breaks = seq(0, 40, 5),
    minor_breaks = seq(0, 40, 1),
    name = "Air temperature (°C)"
  ) +
  
  labs(
    title =NULL,
    subtitle =NULL,
    caption =NULL,
    #"Changes of air temperature in greehouse during the experiment."
  )+
  
  theme_minimal() +
  Theme_Box_line(base_family = "latin-times") +
  theme(
    legend.direction = "horizontal",
    #legend.position = c(0.8, 0.4),
    legend.position = "top",
    legend.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(size = 0.1, color="black"),
    panel.grid.major.y = element_line(size = 0.1, color="black"),
    
  )

p/p1
