##気温，根域温度，蓄熱槽の温度の相互関係を解析する##
#hampel関数によりセンサーの異常値を除去した蓄熱利用時18~6時の気温，根域温度，蓄熱槽の温度を抽出し，
#根域温度の設定値（21℃）と気温，根域温度，蓄熱槽の温度の差（ΔT）を求める
#ΔTの日平均，日積算値をもとめ，気温と根域温度，気温と蓄熱槽の温度，根域温度を蓄熱槽の温度の相互関係を解析した。

library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(hms)
library(stringr)
library(readr)
library(pracma)
library(patchwork)
library(ggrepel) 


source("source/Theme_Box_line.R")
#図の共通スタイルの指定
fig_style <- function() {
  list(
    theme_minimal() +
      Theme_Box_line(base_family = "latin-times") +
      theme(
        legend.direction = "horizontal",
        #legend.position = c(0.8, 0.4),
        legend.position = "top",
        legend.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(size = 0.1, color="black"),
        panel.grid.major.y = element_line(size = 0.1, color="black")
      )
  )
}


#タイムゾーンの指定
tz_tokyo="Asia/Tokyo"

#使用する日時範囲を指定
from <- as.POSIXct("2025-08-01 18:00:00", tz = tz_tokyo) 
to   <- as.POSIXct("2025-09-10 06:00:00", tz = tz_tokyo)
d_from <- as.Date(with_tz(from, tz=tz_tokyo))
d_to <- as.Date(with_tz(to, tz=tz_tokyo))

# Hampel関数でノイズを除去したRDS からデータを読み込み
data_rds <- read_rds("/cloud/project/data/rds/TEMP_Hampel_AT_RT_HT_18_6h.rds")
#RDSのコメント出力
print(data_rds$meta[])

#コメント部分は除いてデータ部分のみを取り出す
df_hampel <- data_rds$data 


#根域温度の設定値21℃との差を求める----
ST= 21#根域温度の設定値（21℃）

df_D_ST <- df_hampel %>%
  mutate(dif_st = Hampel_V-ST) %>%
  select (-is_outlier)

#設定値のΔTの日付ごとの合計と平均値を計算----
sum_t <- df_D_ST %>%
  filter (Date >= d_from, Date <= d_to) %>%
  group_by(Type, Date) %>%
  summarise(sum_dt=sum(dif_st),
            avg=round(mean(dif_st),1),
            n = sum(!is.na(dif_st)),
            .groups = "drop") %>%
  select(Date, Type, sum_dt, avg, n) 

#1日のΔTの平均温度をワイド化-----
df_xy_plot <- sum_t %>%
  select (Date, Type, avg) %>%
  pivot_wider(names_from = Type, values_from = avg)

#作図1：1日の平均ΔTについて気温と根域温度の散布図-----
type_x <- "Air_T"
type_y <- "RZT_W_HP"
pl1<-ggplot(df_xy_plot, aes(x = .data[[type_x]], y = .data[[type_y]])) +
  geom_point(stat = "identity", position = "identity")+
  geom_smooth(method = "auto", se = TRUE) +
  labs(x = type_x, y = type_y)+
  geom_text_repel(aes(label = format(Date, "%m-%d")))+
  fig_style()

#作図2：1日の平均ΔTについて気温と蓄熱槽の温度の散布図-----
type_x <- "Air_T"
type_y <- "HST_W_HP"
pl2<-ggplot(df_xy_plot, aes(x = .data[[type_x]], y = .data[[type_y]])) +
  geom_point(stat = "identity", position = "identity")+
  geom_smooth(method = "auto", se = TRUE) +
  labs(x = type_x, y = type_y)+
  geom_text_repel(aes(label = format(Date, "%m-%d")))+
  fig_style()

#作図3：1日の平均ΔTについて蓄熱槽の温度と根域温度の散布図-----
type_x <- "HST_W_HP"
type_y <- "RZT_W_HP"
pl3<-ggplot(df_xy_plot, aes(x = .data[[type_x]], y = .data[[type_y]])) +
  geom_point(stat = "identity", position = "identity")+
  geom_smooth(method = "auto", se = TRUE) +
  scale_x_continuous(
    limits = c(-8, 0))+
    labs(x = type_x, y = type_y)+
  geom_text_repel(aes(label = format(Date, "%m-%d")))+
  fig_style()