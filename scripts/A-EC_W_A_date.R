# パッケージ読み込み


library(ggplot2)
library(tidyr)
library(dplyr)
library(directlabels)

#ひな形を読み込み
source("source/Theme_Box_line.R")

tz_use <- "Asia/Tokyo"

data <- read.table("/cloud/project/data/EC_water_air_date from 250704 to 0903.csv",sep=",", comment.char="#", header=T) 

data[,1] <- as.POSIXct(data[,1], format="%Y-%m-%d", tz=tz_use) #列１をPOSIXの日時データに変換

# データの整形# データをlong形式に変換（ggplotで扱いやすくするため）
df_long <- data %>% pivot_longer(
                      cols = c("Air_HP_kWh","Water_HP_kWh"),
                       names_to = "Type",
                       values_to = "Value")
#項目の名称変更
df_long <-  df_long %>%
  mutate(Type = recode(Type,
                        "Air_HP_kWh" = "Air",
                        "Water_HP_kWh"    = "Water"))

#起点を0とした差分
t0 <- as.POSIXct("2025-07-04", tz = tz_use) 
baseline_exact <- df_long %>%
  filter(date == t0) %>%
  transmute(Type,
            baseline_time  = date,
            baseline_value = Value)
df_diff_exact <- df_long %>%
  left_join(baseline_exact, by = "Type") %>%
  mutate(df_diff_exact = ifelse(date >= t0, Value - baseline_value, NA_real_))


#表示範囲指定
from <- as.POSIXct("2025-07-04", tz = tz_use) 
to   <- as.POSIXct("2025-08-31", tz = tz_use)
df_diff_exact <-  df_diff_exact %>%
  filter(date >= from, date <= to) |>
  filter(!is.na(Value))


p <- ggplot(df_diff_exact, aes(x = date, y = df_diff_exact, colour=Type)) +
  geom_line(stat = "identity", position = "identity", size = 0.6) +
  scale_color_manual(values = c("Air" = "darkblue","Water" = "red"))+
  
  scale_x_datetime(
                  breaks = "1 week",
                   minor_breaks = NULL,
                   date_labels =  "%b\n%d",
                   name = "2025")+
  
  scale_y_continuous(limits = c(0, 2200),
                     breaks = seq(0, 2200, 500),
                     minor_breaks = NULL,
                     name= "Electric consumption(kWh)")+
  
  theme_minimal()+
  #ひな形を指定
  Theme_Box_line(base_family = "latin-times") +
  
  #オプションのみ指定
  theme(
    legend.position = c(0.3,0.95)

  )

##direct.label(p, "first.qp")
p
# # 画像ファイルとして保存
ggsave("/cloud/project/figs/EC_water_Air_date.pdf", plot = p, width = 10, height = 10, units = "cm", device=cairo_pdf)
#ggsave("/cloud/project/figs/EC_water_Air_date.png", plot = p, width = 10, height = 10, units = "cm", dpi=600, bg = "transparent")

