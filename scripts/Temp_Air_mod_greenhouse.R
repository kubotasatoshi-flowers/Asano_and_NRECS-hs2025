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
data <- readRDS("/cloud/project/data/TEMP_Air_NRECS_all_20250929_mod.rds")

#列1の名前を変更
names(data)[1] <- "datetime"

#データ表示範囲指定
from <- as.POSIXct("2025-07-01 00:00:00", tz = tz_use) 
to   <- as.POSIXct("2025-09-10 23:59:00", tz = tz_use)

# データの整形# データをlong形式に変換
df_long <-
  data %>% pivot_longer(
    cols = -c(datetime),
    names_to = "Type",
    values_to = "Value"
  )



df_day_t <- df_long %>%
  mutate(date = as_date(with_tz(datetime, "Asia/Tokyo"))) %>%  # 日付抽出
  group_by(date, Type) %>%
  summarise(Max = max(Value, na.rm = TRUE),
            Mean = round(mean(Value, na.rm = TRUE),1),
            Min = min(Value, na.rm = TRUE),
            .groups = "drop")

  

#項目の名称変更
# df_long <-  df_long %>%
#   mutate(Type = recode(Type,
#                        "Soil_T2" = "Control of cyclamen",
#                        "Soil_T3" = "Control of garbela",
#                        "Air_T_NRECS" = "N.RECS",
#                        "Air_T_Cont" = "Control"))

#データ表示範囲にデータを整形
df_long <-  df_long %>%
  filter(datetime >= from, datetime <= to) %>%
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

select <- c(
          "Air_T_Cont_new")

# 色テーブル（既存の指定を流用）
col_map <- c(
  "Air_T_Cont_new"   = "darkblue"
          )

p <- ggplot(
  df_long %>%
    filter(Type %in% select),
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
    breaks = seq(from, to, by = "7 days"),
    minor_breaks = seq(from, to, by = "1 days"),
    labels = lab_date_time_once,
    name = "2025"
  ) +
  scale_y_continuous(
    limits = c(0, 40),
    breaks = seq(0, 40, 5),
    minor_breaks = NULL,
    name = "Temperature (°C)"
  ) +
  ggtitle("") +
  theme_minimal() +
  Theme_Box_line(base_family = "latin-times") +
  theme(
    legend.direction = "horizontal",
    legend.position = c(0.2, 0.2),
    legend.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(size = 0.1, color="black"),
    panel.grid.major.y = element_line(size = 0.1, color="black")
  )



# ggsave("/cloud/project/figs/Temp_NRECS_HS_hampel.pdf",
#        plot = p,
#        device=cairo_pdf,
#        width = 10,
#        height = 10,
#        units = "cm")
# 
# ggsave("/cloud/project/figs/Temp_NRECS_HS_original.pdf",
#        plot = p2,
#        device=cairo_pdf,
#        width = 10,
#        height = 10,
#        units = "cm")


