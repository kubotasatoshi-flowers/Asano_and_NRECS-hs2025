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



f <- "TEMP_AirHP_2nd_WT_20250817_1023"   #ファイル名を入力する（.csvは除く）

# 空気熱源式の2次側水温のデータ読み込み
WT <- read.table(paste0("/cloud/project/data/",f,".csv"),
                 sep=",", comment.char="#", header=T) 

#列１をPOSIXの日時データに変換，列1以外を数値データに変換
WT <- WT %>%
  mutate(across(1, ~ as.POSIXct(., format = "%Y-%m-%d %H:%M", tz = tz_tokyo)))
WT <- WT %>%
  mutate(across(2:3, as.numeric)) %>%
  rename(Time_stamp = date_time)


df_long <- WT %>%
  pivot_longer(
    cols = -Time_stamp,
    names_to ="Type",
    values_to ="Value")

#ハンペル関数によるノイズ除去----
# センサーごとに適用（sensor_id が無い場合は group_by を外す）

k  <- 5 # 窓幅（サンプル数ベース）
t0 <- 2    # 閾値（3*MAD 超えを外れ）

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
path="/cloud/project/data/rds/"
file_n = paste0(f,"_Hampel") 
#rdsファイルに入れるコメント
com1 ="2025年8月17日〜10月23日までの空気熱源式HPの2次側の水温センサー"
com2 ="窓数は5,閾値は2とした。実データはOrg_V，Hampelで処理した数値はHampel_V，ロング形式で記録"
o_f = paste0(f,".csv")
#rdsファイルの作成者名
aut ="Satoshi Kubota"

# RDS形式で保存
df_rds <- list(
  data = df_hampel,
  meta = list(
    comment1 = paste0(com1),
    comment2 = paste0(com2),
    original_file = paste0(o_f),
    author = paste0(aut),
    created = Sys.time(),
    tz      = Sys.timezone()
  )
)

saveRDS(df_rds, paste0(path,file_n,".rds"))
