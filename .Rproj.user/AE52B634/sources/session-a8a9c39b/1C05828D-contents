#蓄熱槽の温度を蓄熱利用終了時（6時）と蓄熱併用終了時（18時）の温度差を計算するスクリプト
#タイムゾーンの指定

tz_use <- "Asia/Tokyo"

#csvからデータ読み込み
data <- read.table("/cloud/project/data/TEMP_NRECS_HS.csv",sep=",", comment.char="#", header=T) 

#列1の名前を変更
names(data)[1] <- "datetime"

#列１をPOSIXの日時データに変換
library(tidyr)
library(dplyr)
data <- data %>%
  mutate(across(1, ~ as.POSIXct(., format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Tokyo")))
#列2を数値データに変換
data <- data %>%
  mutate(across(2:5, as.numeric))


#時刻で抽出
library(hms)
times <- as_hms(c("06:00:00", "18:00:00"))
df_time <- data %>%
  filter(as_hms(datetime) %in% times)

#ロング形式に変換
df_long <- df_time%>%
  pivot_longer (
    cols = c("RZT_W_HP","HST_W_HP","RZT_A_HP","HST_A_HP"),
    names_to = "Type",
    values_to = "Value")
#6時で抽出し，日付と前日の日付を付加
t1 <- df_long %>%
  group_by(Type, datetime) %>%
  filter(format(datetime, "%H:%M") == "06:00") %>%
  mutate(day=as.Date(datetime, tz=tz_use), day_1=day-1L) #実際の日付の1日前の日付をday_1に代入

#18時で抽出し，日付を付加
t2 <- df_long %>%
  group_by(Type, datetime) %>%
  filter(format(datetime, "%H:%M") == "18:00") %>%
  mutate(day=as.Date(datetime, tz=tz_use))

#t1とt2をタイプと日付で連結し昼間の温度差を計算
library(stringr)
diff_day <- inner_join (t1, t2, by=c("Type"="Type","day"="day"), #t1:6hour, t2:18hour, 同日の6時と18時の差
                        suffix=(c("_6h","_18h"))) %>%
  mutate (Diff_day = Value_18h - Value_6h) %>%
  filter (str_detect(Type,"HST"))

#t1とt2をタイプと日付と前日の日付で連結し夜間の温度差を計算
diff_night <- inner_join (t1, t2, by=c("Type"="Type","day_1"="day"), #t1:6hour, t2:18hour, 前日の18時と本日の6時の差
                          suffix=(c("_6h","_18h"))) %>%
  mutate (Diff_night = Value_6h - Value_18h) %>%
  filter (str_detect(Type,"HST"))

