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


data <- read.table("/cloud/project/data/Water_HP_in_out_1st_2nd_WT.csv",sep=",", comment.char="#", header=T) 

names(data)[1] <- "datetime"

data[,1] <- as.POSIXct(data[,1], format = "%Y-%m-%d %H:%M:%S", tz="Asia/Tokyo") #列１をPOSIXの日時データに変換

#表示範囲指定
from <- as.POSIXct("2025-08-21 06:00:00", tz = tz_use) 
to   <- as.POSIXct("2025-08-25 06:00:00", tz = tz_use)

# データの整形# データをlong形式に変換（ggplotで扱いやすくするため）
df_long <-
  data |> pivot_longer(
                      cols = c("X1st_out_WT","X1st_in_WT","X2nd_out_WT","X2nd_in_WT"),
                       names_to = "Type",
                       values_to = "Value")

#項目の名称変更
df_long <-  df_long %>%
  mutate(Type = recode(Type,
                       "X1st_out_WT" = "Primary",
                       "X1st_in_WT" = "Primary in",
                       "X2nd_out_WT" = "Secondary out",
                       "X2nd_in_WT" = "Secondary in"
                       ))

df_long <-  df_long %>%
  filter(datetime >= from, datetime <= to) |>
  filter(!is.na(Value))
  
df_long <- df_long %>%
  mutate(
    datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = tz_use)
  )





# 同じ日の2本目以降は日付を空欄にするラベラー
lab_date_time_once <- function(x, tz = tz_use) {
  d <- as.Date(x, tz = tz)
  first <- c(TRUE, d[-1] != d[-length(d)])
  paste0(ifelse(first, format(x, "%m-%d", tz = tz), ""),
         "\n",
         format(x, "%k:%M", tz = tz) |> trimws())
}

breaks_h <- seq(from, to, by = "12 hours")  # POSIXct



p1 <-
  ggplot(df_long, aes(x=datetime)) +
  geom_line(aes(y=Primary, color = "Primary") )
scale_color_manual(values = c("Primary out" = "darkblue","Primary in" = "red"))+

scale_x_datetime(
  limits = c(from, to),
  breaks = breaks_h,
  minor_breaks = NULL,
  labels = lab_date_time_once,   # ← 二行表示＆同日まとめ
  name= "Date Time"
)
+

  scale_y_continuous(limits = c(0, 40),
                     breaks = seq(0, 40, 5),
                     minor_breaks = NULL,
                     name= "Temperature (°C)"
                     )

p2 <-
  
  ggplot(df_long, aes(datetime, Value, colour = Type)) +
  geom_line(stat = "identity", position = "identity", size = 0.6) +
  scale_color_manual(values = c(
                                "Secondary out" = "darkgreen",
                                "Secondary in" = "orange"))+
  
  scale_x_datetime(
    limits = c(from, to),
    breaks = breaks_h,
    minor_breaks = NULL,
    labels = lab_date_time_once,   # ← 二行表示＆同日まとめ
    name= "Date Time"
  ) +
  
  
  scale_y_continuous(limits = c(0, 40),
                     breaks = seq(0, 40, 5),
                     minor_breaks = NULL,
                     name= "Temperature (°C)"
  )

p1/p2
  
  theme_minimal()+
  #ひな形を指定
  Theme_Box_line()+
  
  #オプションのみ指定
  
  theme(
  legend.position = c(0.3,0.98))
  


##direct.label(p, "first.qp")
# # 画像ファイルとして保存
ggsave("/cloud/project/figs/Temp_Water_HP.pdf", plot = p, width = 10, height = 20, units = "cm", device=cairo_pdf)
ggsave("/cloud/project/figs/Temp_Water_HP.png", plot = p, width = 10, height = 20, units = "cm", dpi=600, bg = "transparent")

