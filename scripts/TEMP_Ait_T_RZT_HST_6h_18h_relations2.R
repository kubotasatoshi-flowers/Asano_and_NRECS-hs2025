##気温，根域温度，蓄熱槽の温度の相互関係を解析する##
#蓄熱利用時18~6時の気温，根域温度，蓄熱槽の温度を抽出し，
#hampel関数によりセンサーの異常値を除去する
#直前の気温，根域温度，蓄熱槽の温度が現在の根域温度に影響するとの仮説
#RT_C=RT_P-HS_P+AT_P
#温度の相互関係を解析した。

library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(hms)
library(stringr)
library(readr)
library(pracma)
library(patchwork)
library(ggrepel) 


source("source/Theme_Box_line.R")
#図の共通スタイルの指定
fig_style <- function() {
  list(
    theme_minimal() +
      Theme_Box_line(base_family = "latin-times") +
      theme(
        legend.direction = "horizontal",
        #legend.position = c(0.8, 0.4),
        legend.position = "top",
        legend.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(size = 0.1, color="black"),
        panel.grid.major.y = element_line(size = 0.1, color="black")
      )
  )
}


#タイムゾーンの指定
tz_tokyo="Asia/Tokyo"

#使用する日時範囲を指定
from <- as.POSIXct("2025-08-01 18:00:00", tz = tz_tokyo) 
to   <- as.POSIXct("2025-09-10 06:00:00", tz = tz_tokyo)
d_from <- as.Date(with_tz(from, tz=tz_tokyo))
d_to <- as.Date(with_tz(to, tz=tz_tokyo))

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

#作図：気温，根域温度，蓄熱槽の温度の18~6時の変化-----

select <- c("Air_T",
            "RZT_W_HP",
            "HST_W_HP")

col_map <- c(Air_T = "red",
             RZT_W_HP = "darkgreen",
             HST_W_HP = "blue") 

df_plot <- df_hampel %>%
  filter(Time_stamp >= from, Time_stamp <= to) %>%
  filter(Type %in% select, !is.na(value_clean)) %>%         # ← ここで対象系列だけ残す
  mutate(Type = factor(Type, levels = select))        # ← 凡例順も固定

p1 <- ggplot(df_plot, mapping = aes(x = Time_stamp, y = value_clean, color = Type, group = interaction(Date, Type, drop = TRUE)))+
  geom_line(stat = "identity", position = "identity", size = 0.3) +
  scale_x_datetime(
    limits = c(from, to),
    breaks = seq(from, to, by = "7 days"),
    minor_breaks = seq(from, to, by = "1 days"),
    date_labels = "%b-\n%d",
    name = "2025") +
  scale_color_manual(
    values = col_map,  # 使う色だけ
    breaks = select,           # 凡例の順序と表示対象を制御
    limits = select,
    name   = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 30),
    breaks = seq(0, 30, 5),
    minor_breaks = NULL,
    name = "Temperature (°C)"
  )




#根域温度の設定値21℃との差を求める----
ST= 21

df_D_ST <- df_hampel %>%
  mutate(dif_st = value_clean-ST) %>%
  select (-is_outlier)

#設定値のΔTの日付ごとの合計を計算----
sum_t <- df_D_ST %>%
  filter (Date >= d_from, Date <= d_to) %>%
  group_by(Type, Date) %>%
  summarise(sum_dt=sum(dif_st),
            avg_dt=round(mean(dif_st),1),
            n = sum(!is.na(dif_st)),
            .groups = "drop") %>%
  select(Date, Type, sum_dt, avg_dt, n) %>%
  mutate (avg = round(sum_dt/n,1))

#1日の温度合計をワイド化-----
df_xy_plot <- sum_t %>%
  select (Date, Type, avg_dt) %>%
  pivot_wider(names_from = Type, values_from = avg_dt)

#作図：1日ごとのΔTを気温と根域温度，気温と蓄熱槽の温度，蓄熱槽の温度と根域温度でプロット-----

type_x <- "Air_T"
type_y <- "RZT_W_HP"
pl1<-ggplot(df_xy_plot, aes(x = .data[[type_x]], y = .data[[type_y]])) +
  geom_point(stat = "identity", position = "identity")+
  geom_smooth(method = "auto", se = TRUE) +
  labs(x = type_x, y = type_y)+
  geom_text_repel(aes(label = format(Date, "%m-%d")))

type_x <- "Air_T"
type_y <- "HST_W_HP"
pl2<-ggplot(df_xy_plot, aes(x = .data[[type_x]], y = .data[[type_y]])) +
  geom_point(stat = "identity", position = "identity")+
  geom_smooth(method = "auto", se = TRUE) +
  labs(x = type_x, y = type_y)+
  geom_text_repel(aes(label = format(Date, "%m-%d")))

type_x <- "HST_W_HP"
type_y <- "RZT_W_HP"
pl3<-ggplot(df_xy_plot, aes(x = .data[[type_x]], y = .data[[type_y]])) +
  geom_point(stat = "identity", position = "identity")+
  geom_smooth(method = "auto", se = TRUE) +
  scale_x_continuous(
    limits = c(-8, 0))+
    labs(x = type_x, y = type_y)+
  geom_text_repel(aes(label = format(Date, "%m-%d")))


df_D_ST_plot <- df_D_ST %>%
  select (Time_stamp, Date, Type, dif_st) %>%
  pivot_wider(names_from = Type, values_from = dif_st) %>%
  mutate(A_RZT = Air_T + RZT_W_HP)


type_x <- "HST_W_HP"
type_y <- "A_RZT"
pl4<-ggplot(df_D_ST_plot, aes(x = .data[[type_x]], y = .data[[type_y]])) +
  geom_point(stat = "identity", position = "identity")+
  geom_smooth(method = "auto", se = TRUE) +
  scale_x_continuous(limits = c(-8, 0),
                     
  labs(x = type_x, y = type_y))


  geom_text_repel(aes(label = format(Date, "%m-%d")))


