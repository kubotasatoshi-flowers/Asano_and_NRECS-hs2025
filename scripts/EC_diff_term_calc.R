#消費電力のデータからwater_HPのデータに関して指定期間の消費電力量の差分を1時間ごとに計算し，csv形式で保存


# パッケージ読み込み
library(tidyr)
library(dplyr)
library(readr)
#消費電力量データの読み込み
data <- read.table("/cloud/project/data/EC_water_air_hour from 250704 to 0903.csv",sep=",", comment.char="#", header=T) 
tz_use <- "Asia/Tokyo"
names(data)[1] <- "datetime"
#列１をPOSIXの日時データに変換
data[,1] <- as.POSIXct(data[,1], format = "%Y-%m-%d %H:%M:%S", tz="Asia/Tokyo") 
#日付とwater_HPの消費電力量だけを選択
data <- select(data,c("datetime","Water_HP_kWh"))

#表示範囲指定
from <- as.POSIXct("2025-09-1 06:00:00", tz = tz_use) 
to   <- as.POSIXct("2025-09-04 06:00:00", tz = tz_use)

from_ti <- as.character(format(from,"%m-%d"))
to_ti <- as.character(format(to,"%m-%d"))

Tit <- paste0(from_ti," to ",to_ti)

names(data)[2] <- Tit

# データの整形# データをlong形式に変換
df_long <- data %>%
  pivot_longer(
    cols = all_of(Tit),
    names_to = "Type",
    values_to = "Value"
  ) %>%
  filter(
    datetime >= from,
    datetime <= to,
    !is.na(Value)
  )

#起点を0とした差分
t0 <- from
baseline_exact <- df_long %>%
  filter(datetime == t0) %>%
  transmute(Type,
            baseline_time  = datetime,
            baseline_value = Value)
Diff_EC <- df_long %>%
  left_join(baseline_exact, by = "Type") %>%
  mutate(
         Difference_time = as.numeric(difftime(datetime, baseline_time, units = "hours")),
         Difference_EC = ifelse(datetime >= t0, Value - baseline_value, NA_real_)
         )
#タイムゾーンを文字列化
data_out <- Diff_EC %>%
  mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S", tz = tz_use))

#csvデータの保存
fn <- paste0("/cloud/project/data/EC_dif_",Tit,".csv")
write_csv(data_out, fn)
