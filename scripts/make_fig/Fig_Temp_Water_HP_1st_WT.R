#地下水熱源式HPの1次側の温度をプロットする

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
         "Tank"="Well_Tank_T")  %>%
  mutate (Dif_P = `1st out` - `1st in`, Dif_S = `2nd in`-`2nd out`)

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
    names_to = "Type",
    values_to = "Value"
    )

  
#データ表示範囲にデータを整形
df_long <-  df_long %>%
  filter(Time_stamp >= from, Time_stamp <= to) %>%
  filter(!is.na(Value))

#1次側の水温の作図----
select0 <- c("1st in",
             "1st out",
             "Dif_P")

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
                           "In" = "1st in",
                           "Out" = "1st out",
                           "Difference" = "Dif_P"))


p <- ggplot(df_plot,
            aes(x = Time_stamp,
                y = Value,
                color = Type,
                #group = Type
                group = interaction(Date, Type, drop = TRUE)
            )
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
    limits = c(-5, 45),
    breaks = seq(-5, 45, 5),
    minor_breaks = seq(-5, 45, 1),
    name = "Water temperature (°C)"
  ) +
  
  labs(
    title ="Primary temperature of water HP",
    subtitle =NULL,
    caption =NULL,
  )+
  
  theme_minimal() +
  Theme_Box_line(base_family = "latin-times") +
  theme(
    legend.justification = c("right","top"),
    legend.position = c(1, 1),
    #legend.position = "top",
    #legend.position = "right",
    legend.direction = "horizontal",
    #legend.direction = "vertical",
    legend.box.margin = margin(t = 10, r = 10, b = 6, l = 6, unit = "pt"),
    # legend.margin = margin(t = 1, r = 6, b = 1, l = 6, unit = "pt"),
    # legend.background = element_rect(
    #   fill = "white",
    #   colour = "black",
    #   linewidth = 0.1,    # 旧版なら size = 1.2
    #   linetype = "solid"
    #   ),
    panel.grid.major.x = element_line(size = 0.1, color="black"),
    panel.grid.major.y = element_line(size = 0.1, color="black"),
    
  )


 
 
 ggsave("/cloud/project/figs/Fig_Temp_Water_HP_1st_WT.pdf",
        plot = p,
        device=cairo_pdf,
        width = 15,
        height = 8,
        units = "cm")
