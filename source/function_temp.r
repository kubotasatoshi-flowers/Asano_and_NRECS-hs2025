
#csvからデータ読み込み，comment.charで指定した文字列でコメント行を認識
data <- read.table("/cloud/project/data/Well_time.csv",sep=",", comment.char="#", header=T) 



#列を数値データにする
data$RZT_NRECS_W_HP <- as.numeric(data$RZT_NRECS_W_HP)

#列1の名前を変更
names(data)[1] <- "datetime"


#列１をPOSIXの日時データに変換
data[,2] <- as.POSIXct(data[,2], format = "%Y-%m-%d", tz="Asia/Tokyo")

#列4をPOSIXの時間データに変換
data[,4] <- as_hms(data[,4])

#データフレームから必要な列を選択して新しいデータフレームを作る
data_select <- select(data,2:3)


#ロング形式に変換
df_long <- 
  data_select %>%
  pivot_longer(
    cols=(c("off_seconds","water_kL")),  #データフレームの中で数値にするカラムの名前
    names_to="Items", #項目列の名前（特に変更する必要なし）
    values_to="V" #数値列の名前（特に変更する必要なし）
  )



# 同じ日の2本目以降は日付を空欄にするラベラー
lab_date_time_once <- function(x, tz = tz_use) {
  d <- as.Date(x, tz = tz)
  first <- c(TRUE, d[-1] != d[-length(d)])
  paste0(ifelse(first, format(x, "%m-%d", tz = tz), ""),
         "\n",
         format(x, "%k:%M", tz = tz) |> trimws()) #06:00を6:00にするための処理
}


#項目の名称変更
df_long <-  df_long %>%
  mutate(Type = recode(Type,
                       "Soil_T1" = "RZT at N.RECS",
                       "Well_Tank_T" = "Well tank",
                       "Well_Water_T" = "Well water"))


# 読み込むフォルダを指定
path <- "/cloud/project/data/"
fn <- c("EC_dif_08-16 to 08-19.csv",
        "EC_dif_08-22 to 08-24.csv",
        "EC_dif_08-25 to 08-27.csv",
        "EC_dif_08-28 to 08-31.csv")

# CSVファイル一覧を取得
files <- paste0(path,fn)

# まとめて読み込み → リストになる
df_list <- lapply(files, function(f) {
  read_csv(f, comment="#") %>%
    mutate(source_file = basename(f))   # ファイル名だけ（パスを除く）
  # mutate(source_file = f)           # フルパスを残したい場合はこちら
})

