#N.RECSの各処理区の根域温度を作図する
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

#pdfの出力パス名
path_n ="/cloud/project/figs/"
#pdfの出力ファイル名
pdf_n = "TEMP_NRECS_hs_fig"


#タイムゾーンの指定
tz_use <- "Asia/Tokyo"

# RDS から温度データを読み込み
data_rds <- readRDS("/cloud/project/data/TEMP_Air_NRECS_all_20250929_mod.rds")
data <- data_rds$data #データ部分のみを取り出す

#列1の名前を変更
names(data)[1] <- "datetime"

#データ表示範囲指定
from <- as.POSIXct("2025-07-01 00:00:00", tz = tz_use) 
to   <- as.POSIXct("2025-09-10 23:59:00", tz = tz_use)

# データの整形# データをlong形式に変換
df_long <-
  data %>% 
  filter (datetime >= from, datetime <= to) %>%
  pivot_longer(
    cols = -c(datetime),
    names_to = "Type",
    values_to = "Value"
  )

# #日最高，平均，最低気温を計算
# df_day_t <- df_long %>%
#   mutate(date = floor_date(with_tz(datetime, "Asia/Tokyo"), unit = "day")) %>%  # 日付抽出
#   group_by(date, Type) %>%
#   summarise(Max = max(Value, na.rm = TRUE),
#             Mean = round(mean(Value, na.rm = TRUE),1),
#             Min = min(Value, na.rm = TRUE),
#             .groups = "drop")
# 
# #温室内の気温だけを抽出
# df_day_Air_T <- df_day_t %>%
#   filter (Type %in% "Air_T_Cont_new") %>%
#   select(date, Max, Mean, Min) %>%
#   pivot_longer(
#     cols =-c("date"),
#     names_to = "Type",
#     values_to = "Value"
#   )

# 作図対照だけを抽出
df_long <- df_long %>%
  filter (Type %in%
            c("Cont_Soil_T_Cyclamen",
              "NRECS_hs_whp_Cyclamen",
              "NRECS_hs_ahp_Cyclamen")) %>%
            mutate(Value = na_if(Value, 0)) #0は削除

#作図するデータ系列を指定
select <- c("Cont_Soil_T_Cyclamen",
            "NRECS_hs_whp_Cyclamen",
            "NRECS_hs_ahp_Cyclamen")

#凡例に表示する名称を指定，データ系列の並びと同じ順番とすること
lg_name <- c("Control",
            "Water HP",
            "Air HP")

# 色テーブル
col_map <- c("Cont_Soil_T_Cyclamen" ="darkgreen",
             "NRECS_hs_whp_Cyclamen" = "red",
             "NRECS_hs_ahp_Cyclamen" ="darkblue")

line_map <- c("Cont_Soil_T_Cyclamen" ="solid",
             "NRECS_hs_whp_Cyclamen" = "solid",
             "NRECS_hs_ahp_Cyclamen" ="solid") 

line_w_map<- c("Cont_Soil_T_Cyclamen" =0.1,
               "NRECS_hs_whp_Cyclamen" = 0.3,
               "NRECS_hs_ahp_Cyclamen" = 0.3) 

p <- ggplot(df_long,
  aes(x = datetime, y = Value, color = Type, group = Type, linetype = Type, linewidth = Type)
)+
  geom_line(stat = "identity", position = "identity") +
  #geom_point(stat = "identity", position = "identity", size = 0.5) +
  scale_color_manual(
     values = col_map[select],  # 使う色だけ
     breaks = select,           # 凡例の順序と表示対象を制御
     limits = select,
     labels = lg_name
   ) +
  
  scale_linetype_manual(
    values = line_map[select],  # 線種の指定
    limits = select,
    labels = lg_name
  ) +
  
  scale_linewidth_manual(
    values = line_w_map[select],  # 線種の指定
    limits = select,
    labels = lg_name
  ) +
  
  
  scale_x_datetime(
    limits = c(from, to),
    breaks = seq(from, to, by = "10 days"),
    minor_breaks = seq(from, to, by = "1 days"),
    date_labels = "%b-\n%d",
    name = "2025",
  ) +

  scale_y_continuous(
    limits = c(0, 40),
    breaks = seq(0, 40, 5),
    minor_breaks = seq(0, 40, 1),
    name = "Temperature (°C)"
  ) +
  
  labs(
    title ="N.RECS-hs",
    subtitle =NULL,
    caption =NULL,
      #"Changes of air temperature in greehouse during the experiment."
    )+

  theme_minimal() +
  Theme_Box_line(base_family = "latin-times") +
  theme(
    legend.direction = "horizontal",
    legend.position = c(0.05, 0.2),
    #legend.position = "top",
    legend.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(size = 0.1, color="black"),
    panel.grid.major.y = element_line(size = 0.1, color="black"),

  )

p

ggsave(paste0(path_n,pdf_n, ".pdf"),
       plot = p,
       device=cairo_pdf,
       width = 9,
       height = 10,
       units = "cm")
