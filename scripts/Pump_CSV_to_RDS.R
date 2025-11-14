#空気熱源式と地下水熱源式HPのポンプ流速を取り纏めて，RDSに記録する。
#パルスで記録されており，パルス出力がない時間は記録時間がない。そのため，
#期間中の毎分のデータは以下のように整理する。
#人為的に毎分のTime_stampを生成する。これを親テーブルとしてleft joinでAir_HPとWater_HPの
#Time_stampで結合する。
#Pulse_minがNULLの場合は0を代入する。
#上記でできたデータフレームをRDSとして記録する。

library(dplyr)
library(tidyr)
library(readr)
library(hms)
library(lubridate)

#ファイル名の定義
path="/cloud/project/data/"
file_n1 = "Pump_speed_Air_HP_52C331D7_20250808_1030" #.csvは除いて入力
file_n2 = "Pump_speed_Water_HP_52C331DC_20250809_1030" #.csvは除いて入力
#rdsファイル名
file_n ="Pump_speed_202508008_1030"


#タイムゾーン
tz_use <- "Asia/Tokyo"

#csvからデータ読み込み
data1 <- read.table(paste0(path,file_n1,".csv"), #Air_HP
                   sep=",",
                   comment.char="#",
                   header=T) 
data2 <- read.table(paste0(path,file_n2,".csv"), #Water_HP
                    sep=",",
                    comment.char="#",
                    header=T) 


#csvファイルの内容に合わせて列名やフォーマットを変更する
#列1の名前を変更
names(data1)[2] <- "Time_stamp"
names(data2)[2] <- "Time_stamp"

#列2をPOSIXの日時データに変換
data1 <- data1 %>%
  mutate(across(2, ~ as.POSIXct(., format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Tokyo"))) %>%
  #列3-5を数値データに変換
  mutate(across(3:5, as.numeric)) 

#列2をPOSIXの日時データに変換
data2 <- data2 %>%
  mutate(across(2, ~ as.POSIXct(., format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Tokyo"))) %>%
  #列3-5を数値データに変換
  mutate(across(3:5, as.numeric)) 

#1分ごとの日時データを作成
dt <- data.frame(
  Time_stamp = seq(
    from = as.POSIXct("2025-08-08 00:00:00", tz = "Asia/Tokyo"),
    to   = as.POSIXct("2025-10-30 23:59:59", tz = "Asia/Tokyo"),
    by   = "min"
  ))


d1 <- left_join(dt, data1, by= "Time_stamp") #data1をレフトジョイン
d1 <- d1 %>%
  rename(Pulse_min_Air = Pulse_min, Pulse_sum_Air = Pulse_sum)

d <- left_join(d1,data2,by= "Time_stamp") #data2をレフトジョイン
d <- d %>%
  rename(Pulse_min_Water = Pulse_min, Pulse_sum_Water = Pulse_sum)

data <- d %>% 
  select (Time_stamp, Pulse_min_Air, Pulse_sum_Air, Pulse_min_Water, Pulse_sum_Water)

#rdsファイルに入れるコメント
com1 ="#空気熱源式HPと地下水熱源式HPのポンプスピードのパルス出力2025年8月8日から10月30日まで"
com2 ="200Hz = 10L/min，12000 Pulse/20Lmin"
#rdsファイルの作成者名
aut ="Satoshi Kubota"


# RDS形式で保存
df_rds <- list(
  data = data,
  meta = list(
    comment1 = paste0(com1),
    comment2 = paste0(com2),
    original_file = paste0(file_n,".csv"),
    author = paste0(aut),
    created = Sys.time(),
    tz      = Sys.timezone()
  )
)

saveRDS(df_rds, paste0(path,"rds/",file_n,".rds"))
