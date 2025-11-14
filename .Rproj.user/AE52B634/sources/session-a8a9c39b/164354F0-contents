#消費電力量，ポンプ流速，水温データの読み込みを行い
#一時間当たりの消費電力量，平均流速，平均水温の差を算出
#平均流速，水温差，ブラインの比熱，比重を乗じて1時間当たりの熱量を算出
#熱量を消費電力量で除して，COPを算出
#結果をRDSで保存

# パッケージ読み込み
library(tidyr)
library(dplyr)
library(legendry)
library(scales)
library(crayon)
library(hms)
library(lubridate)
library(readr)

#各種係数
sh = 3.78 #ブライン（プロピレングリコール　37 wt%）10℃のとき，3.78 kJ/kg
sg = 1.04 #ブライン（プロピレングリコール　37 wt%）10℃のとき，1.04
fr_pulse = 1200 #流速1L/minあたり1200パルスとして記録 1L/1200 pulse
kJ_kW = 3600 #3600kJ = 1kW

#タイムゾーンの指定
tz_use <- "Asia/Tokyo"

######消費電力量の読み込み
EC_data <- read.table("/cloud/project/data/Electric_consumption/EC_water_air_hour from 250811 to 0910.csv",sep=",", comment.char="#", header=T) 
EC_data <- EC_data %>%
  rename(Time_stamp = hours) %>%
  mutate(across(1, ~ as.POSIXct(., format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Tokyo"))) %>%   #列1をPOSIXctに変換
  mutate(across(2:3, as.numeric))  #列2-3を数値データに変換

#1時間当たりの消費電力量
EC_hour <- EC_data %>%
  mutate(Delta_A_EC_kWh = Air_hp_kWh - lag(Air_hp_kWh),
         Delta_W_EC_kWh = Water_hp_kWh - lag(Water_hp_kWh)) %>%
  mutate (Date = as_date(Time_stamp),
          Hours = hour(Time_stamp))


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
#空気熱源式HPの1時間ごとの水温データと温度差
WT_A_hour = WT_A_wide %>%
  group_by (Date, Hours) %>%
  summarise (A_mean_in_T = round(mean(Snd_in_WT, na.rm=TRUE),1),
             A_mean_out_T = round(mean(Snd_out_WT, na.rm=TRUE),1),
             .groups ="drop") %>%
  mutate (A_Delta_T = A_mean_in_T - A_mean_out_T)


######地下水熱源式HP2次側水温データ
WT_W_rds <- readRDS("/cloud/project/data/rds/TEMP_Water_HP_1st_2nd_Well_WT_20250811_1023.rds")
WT_W_wide <- WT_W_rds$data #データ部分のみを取り出す
WT_W_wide <- WT_W_wide %>%
  mutate (Date = as_date(Time_stamp),
          Hours = hour(Time_stamp)) 
#地下水熱源式HPの1時間ごとの水温データと温度差
WT_W_hour = WT_W_wide %>%
  group_by (Date, Hours) %>%
  summarise (W_mean_in_T = round(mean(Secondary_in_WT, na.rm=TRUE),1),
             W_mean_out_T = round(mean(Secondary_out_WT, na.rm=TRUE),1),
             .groups ="drop") %>%
  mutate (W_Delta_T = W_mean_in_T - W_mean_out_T)

######空気熱源式HPと地下水熱源式HPの一時間ごとの平均水温とその温度差一覧
WT_A_W <- inner_join(WT_A_hour,
                    WT_W_hour,
                    by=c("Date","Hours")) %>%
  select (Date, Hours, A_mean_in_T, A_mean_out_T, W_mean_in_T, W_mean_out_T, A_Delta_T,W_Delta_T)

######ポンプスピード
PS_rds <- readRDS("/cloud/project/data/rds/Pump_speed_202508008_1030.rds")
PS <- PS_rds$data #データ部分のみを取り出す
PS <- PS %>%
  mutate(Date = as_date(Time_stamp),
         Hours = hour(Time_stamp))

#1時間ごとの平均流速（L/min）
PS_hour <- PS %>%
  group_by (Date, Hours) %>%
  summarise(Mean_A_PS_LM = round (mean(Pulse_min_Air, na.rm = TRUE)/fr_pulse,1),
            Mean_W_PS_LM = round (mean(Pulse_min_Water, na.rm = TRUE)/fr_pulse,1),
            .groups = "drop")

#1時間ごとの水温差と平均流速（L/min）から熱量を求め，消費電力量からCOPを計算する
Heat <- inner_join(WT_A_W, PS_hour, by=c("Date", "Hours")) #水温と流速データの結合
Heat <- inner_join(Heat, EC_hour, by=c("Date", "Hours"))#上記データと消費電力量の結合

Heat <- Heat %>%
  mutate(A_Heat_kW = round((A_Delta_T * Mean_A_PS_LM * 60 * sg * sh/kJ_kW),2),
         W_Heat_kW = round((W_Delta_T * Mean_W_PS_LM * 60 * sg * sh/kJ_kW),2))

Heat <- Heat %>%
  mutate(COP_A = round((A_Heat_kW/Delta_A_EC_kWh),2),
         COP_W = round((W_Heat_kW/Delta_W_EC_kWh),2))




#rdsファイルに入れるコメント
com1 ="2025年8月17日から9月10日までの空気熱源式と地下水熱源式HPにおける消費電力量，熱量，COP"
com2 ="生成コードはCOP_EC_Heat_W_A_hours_20251110.R"
#rdsファイルの作成者名
aut ="Satoshi Kubota"

# RDS形式で保存
df_rds <- list(
  data = Heat,
  meta = list(
    comment1 = paste0(com1),
    comment2 = paste0(com2),
    #original_file = paste0(file_n,".csv"),
    author = paste0(aut),
    created = Sys.time(),
    tz      = Sys.timezone()
  )
)

saveRDS(df_rds, paste0("/cloud/project/data/rds/","COP_EC_Heat_20250817_0910",".rds"))
