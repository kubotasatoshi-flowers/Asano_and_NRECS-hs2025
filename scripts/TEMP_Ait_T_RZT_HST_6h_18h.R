#蓄熱槽の気温と気温を蓄熱利用終了時（6時）と蓄熱併用終了時（18時）でまとめる----

#タイムゾーンの指定
tz_tokyo="Asia/Tokyo"

library(lubridate)
library(dplyr)
library(tidyr)
library(hms)
library(stringr)
library(readr)

from <- as.POSIXct("2025-08-07 18:00:00", tz = tz_tokyo) 
to   <- as.POSIXct("2025-08-10 06:00:00", tz = tz_tokyo)

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
  mutate(Date = if_else (Hours <= as_hms("6:00:00"), Date-days(1), Date))#　6時以前のデータの日付は前日の日付に変換

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

select <- c("Air_T",
            "HST_W_HP",
            "RZT_W_HP")

col_map <- c(Air_T = "red",
             HST_W_HP = "blue",
             RZT_W_HP = "darkgreen") 

df_plot <- df_long %>%
  filter(Time_stamp >= from, Time_stamp <= to) %>%
  filter(Type %in% select, !is.na(Value)) %>%         # ← ここで対象系列だけ残す
  mutate(Type = factor(Type, levels = select))        # ← 凡例順も固定

p <- ggplot(df_plot,
            mapping = aes(x = Time_stamp,
                          y = Value,
                          color = Type,
                          group = interaction(Date, Type, drop = TRUE)))+
  geom_line(stat = "identity", position = "identity")+
  scale_x_datetime(
    limits = c(from, to),
    breaks = seq(from, to, by = "3 days"),
    minor_breaks = seq(from, to, by = "1 days"),
    date_labels = "%b-\n%d",
    name = "2025") +
  scale_color_manual(
    values = col_map,  # 使う色だけ
    breaks = select,           # 凡例の順序と表示対象を制御
    limits = select,
    name   = NULL
  ) 
  

p

