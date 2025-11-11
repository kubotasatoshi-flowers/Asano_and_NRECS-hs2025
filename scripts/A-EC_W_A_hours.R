# パッケージ読み込み
library(ggplot2)
library(tidyr)
library(dplyr)
library(legendry)
library(scales)
library(crayon)


#ひな形を読み込み
source("source/Theme_Box_line.R")

tz_use <- "Asia/Tokyo"


data <- read.table("/cloud/project/data/EC_water_air_hour from 250704 to 0903.csv",sep=",", comment.char="#", header=T) 

names(data)[1] <- "datetime"

data[,1] <- as.POSIXct(data[,1], format = "%Y-%m-%d %H:%M:%S", tz="Asia/Tokyo") #列１をPOSIXの日時データに変換

#表示範囲指定
from <- as.POSIXct("2025-08-28 06:00:00", tz = tz_use) 
to   <- as.POSIXct("2025-08-31 06:00:00", tz = tz_use)

# データの整形# データをlong形式に変換（ggplotで扱いやすくするため）
df_long <-
  data |> pivot_longer(
                      cols = c("Air_HP_kWh","Water_HP_kWh"),
                       names_to = "Type",
                       values_to = "Value")

#項目の名称変更
df_long <-  df_long %>%
  mutate(Type = recode(Type,
                        "Air_HP_kWh" = "Air",
                        "Water_HP_kWh"    = "Water"))

df_long <-  df_long %>%
  filter(datetime >= from, datetime <= to) |>
  filter(!is.na(Value))
  
df_long <- df_long %>%
  mutate(
    datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = tz_use)
  )


#起点を0とした差分
t0 <- from
baseline_exact <- df_long %>%
  filter(datetime == t0) %>%
  transmute(Type,
            baseline_time  = datetime,
            baseline_value = Value)
df_diff_exact <- df_long %>%
  left_join(baseline_exact, by = "Type") %>%
  mutate(df_diff_exact = ifelse(datetime >= t0, Value - baseline_value, NA_real_))





# 同じ日の2本目以降は日付を空欄にするラベラー
lab_date_time_once <- function(x, tz = tz_use) {
  d <- as.Date(x, tz = tz)
  first <- c(TRUE, d[-1] != d[-length(d)])
  paste0(ifelse(first, format(x, "%m-%d", tz = tz), ""),
         "\n",
         format(x, "%k:%M", tz = tz) |> trimws())
}

breaks_h <- seq(from, to, by = "12 hours")  # POSIXct



p <-
  
  ggplot(df_diff_exact, aes(datetime, df_diff_exact, colour = Type)) +
  geom_line(stat = "identity", position = "identity", size = 0.6) +
  scale_color_manual(values = c("Air" = "darkblue","Water" = "red"))+
  
  scale_x_datetime(
    limits = c(from, to),
    breaks = breaks_h,
    minor_breaks = NULL,
    labels = lab_date_time_once,   # ← 二行表示＆同日まとめ
    name= "2025"
  ) +


  scale_y_continuous(limits = c(0, 150),
                     breaks = seq(0, 150, 10),
                     minor_breaks = NULL,
                     name= "Electric consumption(kWh)"
                     )+
  
  theme_minimal()+
  #ひな形を指定
  Theme_Box_line(base_family = "latin-times") +
  
  #オプションのみ指定
  
  theme(
  legend.position= c(0.3,0.95))
  


##direct.label(p, "first.qp")
p
# # 画像ファイルとして保存
ggsave("/cloud/project/figs/EC_water_Air_hour_0828_0831.pdf", plot = p, width = 10, height = 10, units = "cm", device=cairo_pdf)

