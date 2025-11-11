#空気熱源式HPの吸気側と排気側の平均気温をプロットする

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(readr)
library(ggpp)
library(forcats)

#ひな形を読み込み
source("source/Theme_Box_line.R")

#タイムゾーンの指定
tz_use <- "Asia/Tokyo"

# RDS から温度データを読み込み
data_rds <- readRDS("/cloud/project/data/rds/TEMP_Air_HP_in_out_1st_AT_avg_1_2_20250701_1001.rds")
data <- data_rds$data #データ部分のみを取り出す

#各室外機における排気側と吸気側の差の計算
data <- data %>%
  mutate(Dif1 = Outlet_1_AT-Inlet_1_AT, Dif2 =Outlet_2_AT-Inlet_2_AT, Dif_avg=Outlet_avg - Inlet_avg) 

#データ表示範囲指定
from <- as.POSIXct("2025-08-11 00:00:00", tz = tz_use) 
to   <- as.POSIXct("2025-09-10 23:59:00", tz = tz_use)

# データの整形# データをlong形式に変換
df_long <-
  data %>% pivot_longer(
    cols = -Time_stamp,
    names_to = "Type",
    values_to = "Value"
    )

  
#データ表示範囲にデータを整形
df_long <-  df_long %>%
  filter(Time_stamp >= from, Time_stamp <= to) %>%
  filter(!is.na(Value))


#2つの室外機の平均値の作図----
select0 <- c("Inlet_avg",
            "Outlet_avg",
            "Dif_avg")

select1 <- c("In",
             "Out",
             "Difference")

# 色テーブル
col_map <- c("In" ="darkblue",
             "Out" = "red",
             "Difference" = "darkgreen")

df_plot <- df_long %>%
  filter(Type %in% select0, !is.na(Value)) %>%         # ← ここで対象系列だけ残す
  mutate(Type = factor(Type, levels = select0)) %>%         # ← 凡例順も固定
  mutate(Type = fct_recode(Type,
                           "In" = "Inlet_avg",
                           "Out" = "Outlet_avg",
                           "Difference" = "Dif_avg"))


p <- ggplot(df_plot,
            aes(x = Time_stamp, y = Value, color = Type, group = Type)
)+
  geom_line(stat = "identity", position = "identity", size = 0.3) +
  #geom_point(stat = "identity", position = "identity", size = 0.5) +
  scale_color_manual(
    values = col_map,  # 使う色だけ
    breaks = select1,   # 凡例の順序と表示対象を制御
    limits = select1,
    name   = NULL
  ) +
  scale_x_datetime(
    limits = c(from, to),
    breaks = seq(from, to, by = "7 days"),
    minor_breaks = seq(from, to, by = "1 days"),
    date_labels = "%b-%d",
    name = "2025",
  ) +
  
  scale_y_continuous(
    limits = c(-1, 45),
    breaks = seq(0, 45, 5),
    minor_breaks = seq(-1, 40, 1),
    name = "Air temperature (°C)"
  ) +
  
  labs(
    title ="Avereage temperature of 2 outdoor units",
    subtitle =NULL,
    caption =NULL
  )+
  
  theme_minimal() +
  Theme_Box_line(base_family = "latin-times") +
  theme(
    legend.direction = "horizontal",
    legend.position = c(0.58, 0.988),
    #legend.position = "right",
    legend.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(size = 0.1, color="black"),
    panel.grid.major.y = element_line(size = 0.1, color="black"),
    
  )



#室外機1を作図----
select0 <- c("Inlet_1_AT",
             "Outlet_1_AT",
             "Dif1")

select1 <- c("In",
             "Out",
             "Difference")

# 色テーブル
col_map <- c("In" ="darkblue",
             "Out" = "red",
             "Difference" = "darkgreen")

df_plot1 <- df_long %>%
  filter(Type %in% select0, !is.na(Value)) %>%         # ← ここで対象系列だけ残す
  mutate(Type = factor(Type, levels = select0)) %>%         # ← 凡例順も固定
  mutate(Type = fct_recode(Type,
                           "In" = "Inlet_1_AT",
                           "Out" = "Outlet_1_AT",
                           "Difference" = "Dif1"))


p1 <- ggplot(df_plot1,
             aes(x = Time_stamp, y = Value, color = Type, group = Type)
)+
  geom_line(stat = "identity", position = "identity", size = 0.3) +
  #geom_point(stat = "identity", position = "identity", size = 0.5) +
  scale_color_manual(
    values = col_map,  # 使う色だけ
    breaks = select1,   # 凡例の順序と表示対象を制御
    limits = select1,
    name   = NULL
  ) +
  scale_x_datetime(
    limits = c(from, to),
    breaks = seq(from, to, by = "7 days"),
    minor_breaks = seq(from, to, by = "1 days"),
    date_labels = "%b-%d",
    name = "2025",
  ) +
  
  scale_y_continuous(
    limits = c(-1, 45),
    breaks = seq(0, 45, 5),
    minor_breaks = seq(-1, 40, 1),
    name = "Air temperature (°C)"
  ) +
  
  labs(
    title ="Temperature of 1st outdoor unit",
    subtitle =NULL,
    caption =NULL,
    #"Changes of air temperature in greehouse during the experiment."
  )+
  
  theme_minimal() +
  Theme_Box_line(base_family = "latin-times") +
  theme(
    legend.direction = "horizontal",
    legend.position = c(0.338, 0.988),
    #legend.position = "right",
    legend.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(size = 0.1, color="black"),
    panel.grid.major.y = element_line(size = 0.1, color="black"),
    
  )


#室外機2を作図----
select0 <- c("Inlet_2_AT",
             "Outlet_2_AT",
             "Dif2")

select1 <- c("In",
             "Out",
             "Difference")

# 色テーブル
col_map <- c("In" ="darkblue",
             "Out" = "red",
             "Difference" = "darkgreen")

df_plot2 <- df_long %>%
  filter(Type %in% select0, !is.na(Value)) %>%         # ← ここで対象系列だけ残す
  mutate(Type = factor(Type, levels = select0)) %>%         # ← 凡例順も固定
  mutate(Type = fct_recode(Type,
                           "In" = "Inlet_2_AT",
                           "Out" = "Outlet_2_AT",
                           "Difference" = "Dif2"))


p2 <- ggplot(df_plot2,
             aes(x = Time_stamp, y = Value, color = Type, group = Type)
)+
  geom_line(stat = "identity", position = "identity", size = 0.3) +
  #geom_point(stat = "identity", position = "identity", size = 0.5) +
  scale_color_manual(
    values = col_map,  # 使う色だけ
    breaks = select1,   # 凡例の順序と表示対象を制御
    limits = select1,
    name   = NULL
  ) +
  scale_x_datetime(
    limits = c(from, to),
    breaks = seq(from, to, by = "7 days"),
    minor_breaks = seq(from, to, by = "1 days"),
    date_labels = "%b-%d",
    name = "2025",
  ) +
  
  scale_y_continuous(
    limits = c(-1, 45),
    breaks = seq(0, 45, 5),
    minor_breaks = seq(-1, 40, 1),
    name = "Air temperature (°C)"
  ) +
  
  labs(
    title ="Temperature of 2nd outdoor unit",
    subtitle =NULL,
    caption =NULL,
    #"Changes of air temperature in greehouse during the experiment."
  )+
  
  theme_minimal() +
  Theme_Box_line(base_family = "latin-times") +
  theme(
    legend.direction = "horizontal",
    legend.position = c(0.338, 0.988),
    #legend.position = "right",
    legend.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(size = 0.1, color="black"),
    panel.grid.major.y = element_line(size = 0.1, color="black"),
    
  )

#p
#p1
#p2

ggsave("/cloud/project/figs/Fig_Temp_Air_HP_AT_avg.pdf",
       plot = p,
       device=cairo_pdf,
       width = 15,
       height = 10,
       units = "cm")

ggsave("/cloud/project/figs/Fig_Temp_Air_HP_AT_1.pdf",
       plot = p1,
       device=cairo_pdf,
       width = 10,
       height = 10,
       units = "cm")

ggsave("/cloud/project/figs/Fig_Temp_Air_HP_AT_2.pdf",
       plot = p2,
       device=cairo_pdf,
       width = 10,
       height = 10,
       units = "cm")
