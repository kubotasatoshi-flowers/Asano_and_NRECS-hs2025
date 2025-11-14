# パッケージ読み込み
library(ggplot2)
library(tidyr)
library(dplyr)
library(legendry)
library(scales)
library(crayon)

#各種係数
sh = 3.78 #ブライン（プロピレングリコール　37 wt%）10℃のとき，3.78 kJ/kg
sg = 1.04 #ブライン（プロピレングリコール　37 wt%）10℃のとき，1.04
fr_pulse = 1200 #流速1L/minあたり1200パルスとして記録 1L/1200 pulse

#ひな形を読み込み
source("source/Theme_Box_line.R")

tz_use <- "Asia/Tokyo"

EC_data <- read.table("/cloud/project/data/Electric_consumption/EC_water_air_hour from 250811 to 0910.csv",sep=",", comment.char="#", header=T) 
names(data)[1] <- "Time_stamp"
EC_data <- EC_data %>%
  mutate(across(1, ~ as.POSIXct(., format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Tokyo")))
#列2を数値データに変換
EC_data <- EC_data %>%
  mutate(across(2:3, as.numeric))
#1時間当たりの消費電力量
EC_data <- EC_data %>%
  mutate(Delta_A_kWh = Air_hp_kWh -lag(Air_hp_kWh),
         Delta_W_kWh = Water_hp_kWh -lag(Water_hp_kWh))

#日付と時間を抽出して，6時から18時までの消費電力量を抽出
EC_data_6_18 <- EC_data %>%
  mutate(Date = as_date(Time_stamp, tz = "Asia/Tokyo")) %>%
  mutate (Hours = as_hms(Time_stamp))%>%
  filter (Hours >= as_hms("6:00:00") & Hours <= as_hms("18:00:00"))

EC_daytime <- data_6_18 %>%
  group_by(Date)  %>%
  summarise(Air = max(Air_hp_kWh, na.rm = TRUE)-min(Air_hp_kWh, na.rm = TRUE),
            Water = max(Water_hp_kWh, na.rm = TRUE)-min(Water_hp_kWh, na.rm = TRUE),
            .groups = "drop")

#空気熱源式HP2次側水温データ
WT_A_rds <- readRDS("/cloud/project/data/rds/TEMP_AirHP_2nd_WT_20250817_1023_Hampel.rds")
WT_A <- WT_A_rds$data #データ部分のみを取り出す
WT_A_wide <- WT_A %>%
  select (Time_stamp, Type, Hampel_V) 
WT_A_wide <- WT_A_wide %>%
  pivot_wider(names_from = "Type",
              values_from = "Hampel_V")
WT_A_wide <- WT_A_wide %>%
  mutate (Date = as_date(Time_stamp),
          Hours = hour(Time_stamp)) 
#1時間ごとの水温データ
WT_A_hour = WT_A_wide %>%
  group_by (Date, Hours) %>%
  summarise (mean_in = round(mean(Snd_in_WT, na.rm=TRUE),1),
             mean_out = round(mean(Snd_out_WT, na.rm=TRUE),1),
             .groups ="drop") %>%
  mutate (Delta_T = mean_in - mean_out)

WT_A_6_18 <- WT_A_wide %>%
  mutate(Date = as_date(Time_stamp, tz = "Asia/Tokyo")) %>%
  mutate (Hours = as_hms(Time_stamp))%>%
  filter (Hours >= as_hms("6:00:00") & Hours <= as_hms("18:00:00")) %>%
  mutate(Delta_T = Snd_in_WT - Snd_out_WT) %>%
  mutate (Heat_m = Delta_T*sh*sg*(fr/fr_pulse))
          
WT_A_delta <- WT_A_wide

#ポンプスピード
PS_rds <- readRDS("/cloud/project/data/rds/Pump_speed_202508008_1030.rds")
PS <- PS_rds$data #データ部分のみを取り出す

PS <- PS %>%
  mutate(Date = as_date(Time_stamp),
         Hours = hour(Time_stamp))

#1時間ごとの平均流速（L/min）
PS_hour <- PS %>%
  group_by (Date, Hours) %>%
  summarise(Mean_Air_PS_LM = round (mean(Pulse_min_Air, na.rm = TRUE)/1200,1),
            Mean_Water_PS_LM = round (mean(Pulse_min_Water, na.rm = TRUE)/1200,1),
            .groups = "drop")







#表示範囲指定
from <- as.POSIXct("2025-08-11 00:00:00", tz = tz_use) 
to   <- as.POSIXct("2025-09-10 23:59:00", tz = tz_use)

# データの整形# データをlong形式に変換（ggplotで扱いやすくするため）
df_long <-
  data %>% pivot_longer(
                      cols = c("Air_hp_kWh","Water_hp_kWh"),
                       names_to = "Type",
                       values_to = "Value")

#項目の名称変更
df_long <-  df_long %>%
  mutate(Type = recode(Type,
                        "Air_hp_kWh" = "Air",
                        "Water_hp_kWh"    = "Water"))

df_long <-  df_long %>%
  filter(Time_stamp >= from, Time_stamp <= to) %>%
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

