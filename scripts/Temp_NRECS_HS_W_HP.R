library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(readr)
library(ggpp)
library(pracma)

source("source/Theme_Box_line.R")

#タイムゾーンの指定
tz_use <- "Asia/Tokyo"

#csvからデータ読み込み
data <- read.table("/cloud/project/data/TEMP_NRECS_HS.csv",sep=",", comment.char="#", header=T) 

#列1の名前を変更
names(data)[1] <- "datetime"

#列１をPOSIXの日時データに変換
#data[,1] <- as.POSIXct(data[,1], format = "%Y-%m-%d %H:%M:%S", tz="Asia/Tokyo")

data <- data %>%
  mutate(across(1, ~ as.POSIXct(., format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Tokyo")))

data <- data %>%
  mutate(across(2:5, as.numeric))



#データ表示範囲指定
from <- as.POSIXct("2025-08-15 00:00:00", tz = tz_use) 
to   <- as.POSIXct("2025-08-31 23:59:00", tz = tz_use)

# データの整形# データをlong形式に変換
df_long <-data %>%
  pivot_longer(
    cols = c("RZT_W_HP","HST_W_HP","RZT_A_HP","HST_A_HP"),
    names_to = "Type",
    values_to = "Value"
  )

#データ表示範囲にデータを整形
df_long <-  df_long %>%
  filter(datetime >= from, datetime <= to) |>
  filter(!is.na(Value))

# センサーごとに適用（sensor_id が無い場合は group_by を外す）
#ハンペル関数によるノイズ除去
k  <- 30 # 窓幅（サンプル数ベース）
t0 <- 3    # 閾値（3*MAD 超えを外れ）

df_hampel <- df_long %>%
  arrange(Type, datetime) %>%
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

#項目の名称変更
# df_hampel <-  df_hampel %>%
#   mutate(Type = recode(Type,
#                        "RZT_W_HP" = "Root-zone with water heat pump",
#                        "HST_W_HP" = "Heat strage with water heat pump",
#                        "RZT_A_HP" = "Root-zone with air heat pump",
#                        "HST_A_HP" = "Heat strage with air heat pump"))

# 同じ日の2本目以降は日付を空欄にするラベラー
lab_date_time_once <- function(x, tz = tz_use) {
  d <- as.Date(x, tz = tz)
  first <- c(TRUE, d[-1] != d[-length(d)])
  paste0(ifelse(first, format(x, "%m-%d", tz = tz), ""))
  # ,
  # "\n",
  # format(x, "%k:%M", tz = tz) |> trimws()) #06:00を6:00にするための処理
}

#
breaks_h <- seq(from, to, by = "12 hours")  # POSIXct

select <- c(
          "HST_W_HP",
          "HST_A_HP")

# 色テーブル（既存の指定を流用）
col_map <- c(
          "HST_W_HP"   = "darkblue",
          "HST_A_HP"    = "red"
          )

p <- ggplot(
  df_hampel |> dplyr::filter(Type %in% select),
  aes(x = datetime, y = value_clean, color = Type, group = Type)
)+
  geom_line(stat = "identity", position = "identity", size = 0.3) +
  scale_color_manual(
    values = col_map[select],  # 使う色だけ
    breaks = select,           # 凡例の順序と表示対象を制御
    limits = select,
    name   = NULL
  ) +
  scale_x_datetime(
    limits = c(from, to),
    breaks = seq(from, to, by = "3 days"),
    minor_breaks = seq(from, to, by = "1 days"),
    labels = lab_date_time_once,
    name = "2025"
  ) +
  scale_y_continuous(
    limits = c(0, 30),
    breaks = seq(0, 30, 5),
    minor_breaks = NULL,
    name = "Temperature (°C)"
  ) +
  ggtitle("") +
  theme_minimal() +
  Theme_Box_line(base_family = "latin-times") +
  theme(
    legend.direction = "horizontal",
    legend.position = c(0.2, 0.2),
    legend.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(size = 0.1, color="black"),
    panel.grid.major.y = element_line(size = 0.1, color="black")
  )




ggsave("/cloud/project/figs/Temp_NRECS_HS_W_HP.pdf",
       plot = p,
       device=cairo_pdf,
       width = 10,
       height = 10,
       units = "cm")

ggsave("/cloud/project/figs/Temp_NRECS_HS_original.pdf",
       plot = p2,
       device=cairo_pdf,
       width = 10,
       height = 10,
       units = "cm")


