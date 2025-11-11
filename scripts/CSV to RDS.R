#csvファイルをRDSファイルに変更するスクリプト20251002

library(tidyr)
library(dplyr)


#ファイル名の定義
path="/cloud/project/data/"
file_n = "TEMP_NRECS_HS" #.csvは除いて入力
#rdsファイルに入れるコメント
com ="2025年7月1日〜9月10日までのN.RECS-hsの根域温度と蓄熱槽内の温度変化"
#rdsファイルの作成者名
aut ="Satoshi Kubota"

#タイムゾーン
tz_use <- "Asia/Tokyo"

#csvからデータ読み込み
data <- read.table(paste0(path,file_n,".csv"),sep=",", comment.char="#", header=T) 


#csvファイルの内容に合わせて列名やフォーマットを変更する
#列1の名前を変更
names(data)[1] <- "datetime"

#列１をPOSIXの日時データに変換
data <- data %>%
  mutate(across(1, ~ as.POSIXct(., format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Tokyo")))
#列2を数値データに変換
data <- data %>%
  mutate(across(2:5, as.numeric))

# RDS形式で保存
df_rds <- list(
  data = data,
  meta = list(
    comment = paste0(com),
    author = paste0(aut),
    created = Sys.time(),
    tz      = Sys.timezone()
  )
)

saveRDS(df_rds, paste0(path,file_n,".rds"))