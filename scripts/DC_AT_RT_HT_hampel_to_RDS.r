library(lubridate)
library(dplyr)
library(tidyr)
library(hms)
library(stringr)
library(readr)
library(pracma)
library(patchwork)
#library(ggrepel) 

#タイムゾーンの指定
tz_tokyo="Asia/Tokyo"


#-----温室の気温の18時〜翌6時までの平均気温の計算----

# RDS から温室温度データを読み込み
data_rds_g <- read_rds("/cloud/project/data/rds/TEMP_Air_NRECS_all_20250929_mod.rds")

#コメント部分は除いてデータ部分のみを取り出す
data_g <- data_rds_g$data 

#温室の気温データのみとし，日付だけの列を作成し18時〜6時までのデータを抽出
data_g <- data_g %>%
  select (Time_stamp, Air_T_Cont_new) %>%
  mutate(Date = as.Date(with_tz(Time_stamp, tz=tz_tokyo)))%>%
  mutate (Hours = as_hms(Time_stamp)) %>%
  filter (Hours <= as_hms("6:00:00") | Hours >= as_hms("18:00:00")) %>%
  mutate(Date = if_else (Hours <= as_hms("6:00:00"), Date-days(1), Date)) #　6時以前のデータの日付は前日の日付に変換


#-----蓄熱槽の18時と6時の温度を抽出----
# RDS から蓄熱槽温度データを読み込み
data_rds_hs <- read_rds("/cloud/project/data/rds/TEMP_NRECS_HS.rds")
data_hs <- data_rds_hs$data %>% #データ部分のみを取り出す
  rename(Time_stamp = datetime)


#蓄熱槽のデータで日付だけの列を作成し18時から6時のデータを抽出
data_hs <- data_hs %>%
  mutate(Date = as.Date(with_tz(Time_stamp, tz=tz_tokyo)))%>%
  mutate (Hours = as_hms(Time_stamp))%>%
  filter (Hours <= as_hms("6:00:00") | Hours >= as_hms("18:00:00"))

#-----気温と蓄熱槽のデータをジョイン----
df_all <-
  inner_join(data_g,data_hs, by="Time_stamp") %>%
  select (Time_stamp,  Date.y, Hours.x, Air_T_Cont_new, RZT_W_HP, HST_W_HP, RZT_A_HP, HST_A_HP) %>%
  rename(Date =Date.y, Hours =Hours.x, Air_T = Air_T_Cont_new) 


df_long <- df_all %>%
  pivot_longer(
    cols = c("Air_T", "RZT_W_HP", "HST_W_HP", "RZT_A_HP", "HST_A_HP"),
    names_to ="Type",
    values_to ="Value")

#ハンペル関数によるノイズ除去----
# センサーごとに適用（sensor_id が無い場合は group_by を外す）

k  <- 30 # 窓幅（サンプル数ベース）
t0 <- 10    # 閾値（3*MAD 超えを外れ）

df_hampel <- df_long %>%
  arrange(Type, Time_stamp) %>%
  group_by(Type) %>%
  mutate({
    v <- Value
    mask <- !is.na(v)                     # NA でない位置
    
    # 有効サンプルが少ないときはスキップ
    if (sum(mask) >= 3) {
      # 実際に使う窓幅（奇数＆系列長未満に調整）
      k_eff <- min(k, sum(mask) - 1)
      if (k_eff %% 2 == 0) k_eff <- k_eff - 1
      if (k_eff < 1) k_eff <- 1
      
      # NA を除いたサブ系列で Hampel
      h <- hampel(v[mask], k = k_eff, t0 = t0)
      
      # 外れ位置（サブ系列のインデックス）→ 元系列インデックスへ復元
      idx_sub <- if (!is.null(h$ind)) h$ind else integer(0)
      is_outlier <- rep(FALSE, length(v))
      if (length(idx_sub)) {
        idx_full <- which(mask)[idx_sub]
        is_outlier[idx_full] <- TRUE
      }
      
      # 置換後系列（h$y はサブ系列長）
      y_full <- v
      if (!is.null(h$y)) y_full[mask] <- h$y
      
      value_clean <- ifelse(is_outlier, y_full, v)
    } else {
      # データが少ない/全NA のときはそのまま返す
      is_outlier <- rep(FALSE, length(v))
      value_clean <- v
    }
    
    tibble(is_outlier, value_clean)
  }) %>%
  ungroup()

df_hampel <- df_hampel %>%
  rename(Org_V = Value, Hampel_V = value_clean) 


#ファイル名の定義
path="/cloud/project/data/"
file_n = "TEMP_Hampel_AT_RT_HT_18_6h" #.csvは除いて入力
#rdsファイルに入れるコメント
com ="2025年7月1日〜9月10日までの気温，根域温度蓄熱槽の温度を18~6時のデータを抽出し，センサーの異常値をhampel関数で除去してある。実データはOrg_V，Hampelで処理した数値はHampel_Vである。データはロング形式で記録した。"
#rdsファイルの作成者名
aut ="Satoshi Kubota"

# RDS形式で保存
df_rds <- list(
  data = df_hampel,
  meta = list(
    comment = paste0(com),
    author = paste0(aut),
    created = Sys.time(),
    tz      = Sys.timezone()
  )
)

saveRDS(df_rds, paste0(path,file_n,".rds"))

  