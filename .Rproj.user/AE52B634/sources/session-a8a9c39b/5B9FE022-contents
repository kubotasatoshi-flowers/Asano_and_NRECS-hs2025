#スマートガーデナーの標準気温センサーの値が不正確であったため，7月30日に新しいセンサーを接続した。
#新センサー取り付け前の気温を以下の係数で計算し，7月1日~7月31日までは予測値をAir_T_Contに代入した
#ｘ標準気温センサー,ｙは新センサーの値
#y = 1.0379*(x)-3.141
#期間中の気温データをAir_T_Cont_new列に格納し，その他の温度データとともにcsv出力する。

# パッケージ読み込み
library(ggplot2)
library(tidyr)
library(dplyr)
library(legendry)
library(scales)
library(crayon)
library(hms)
library(readr)

tz_use <- "Asia/Tokyo"

#表示範囲指定
from <- as.POSIXct("2025-07-01 00:00:00", tz = tz_use) 
to   <- as.POSIXct("2025-07-29 23:59:59", tz = tz_use)
Path_name ="/cloud/project/data/"
file_name ="TEMP_Air_NRECS_all_20250929" #.csvまでのファイル名を指定
fn <- paste0(Path_name, file_name)

data <- read.table(paste0(fn,".csv"),
                   sep=",",
                   comment.char="#",
                   header=T) 

data_format <-  data %>%
  mutate(Time_stamp = as.POSIXct(Time_stamp, format = "%Y-%m-%d %H:%M:%S", tz="Asia/Tokyo"))%>%
  mutate(across(2:13, ~ suppressWarnings(as.numeric(.x)))) 

#標準温度データが異常な期間の気温を予測値に変換
data_correct <- data_format %>%
  filter(Time_stamp >= from, Time_stamp <= to )%>%
  mutate(Air_T_Cont_mod = round(1.037*Air_T_Cont_old-3.141,1))

#新センサー設置期間のデータを抽出
data_org <- data_format %>%
  filter(Time_stamp > to)

mod_data <- left_join(data_format, data_correct, by = "Time_stamp") %>%
  mutate(Air_T_Cont_new = coalesce(Air_T_Cont_mod, Air_T_Cont.x))%>% #データをマージする
  select(Time_stamp,
         #Air_T_Cont_old = Air_T_Cont_old.x,
         #Air_T_Cont = Air_T_Cont.x,
         Air_T_Cont_new,
         Cont_Soil_T_Cyclamen = Cont_Soil_T_Cyclamen.x,
         Cont_Soil_T_Gerbela = Cont_Soil_T_Gerbela.x,
         NRECS_st_whp_Cyclamen = NRECS_st_whp_Cyclamen.x,
         NRECS_hs_whp_Cyclamen = NRECS_hs_whp_Cyclamen.x,
         NRECS_ug_whp_Gerbela = NRECS_ug_whp_Gerbela.x,
         NRECS_st_ahp_Cyclamen = NRECS_st_ahp_Cyclamen.x,
         NRECS_hs_ahp_Cyclamen = NRECS_hs_ahp_Cyclamen.x,
         NRECS_ug_ahp_Gerbela = NRECS_ug_ahp_Gerbela.x
         )


# RDS形式で保存
df_rds <- list(
  data = mod_data,
  meta = list(
    comment ="2025年N.RECS-hsの実験およびガーデンシクラメンの栽培期間中の温室内気温とコントロール側の根域温度（T3）およびN.RECS側の根域温度，標準の気温センサーには不具合が発覚したため，7月下旬に新センサーを設置した。7月中の気温は新センサーと旧センサーの相関から予測した数値を代入してある。その他の期間は新センサーの数値となっている",
    author ="窪田聡"
  )
)

saveRDS(df_rds, paste0(fn,"_mod.rds"))