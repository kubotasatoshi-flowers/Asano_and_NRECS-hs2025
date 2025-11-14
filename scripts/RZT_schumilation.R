library(dplyr)
#タイムゾーンの指定
tz_tokyo="Asia/Tokyo"

library(lubridate)
library(dplyr)
library(tidyr)
library(hms)
library(stringr)
library(readr)
library(ggplot2)

from <- as.POSIXct("2025-08-12 18:00:00", tz = tz_tokyo) 
to   <- as.POSIXct("2025-08-20 06:00:00", tz = tz_tokyo)

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
  mutate(Date = if_else (Hours <= as_hms("6:00:00"), Date-days(1), Date))

#-----蓄熱槽の18時と6時の温度を抽出----
# RDS から蓄熱槽温度データを読み込み
data_rds_hs <- read_rds("/cloud/project/data/rds/TEMP_NRECS_HS.rds")
data_hs <- data_rds_hs$data %>% #データ部分のみを取り出す
  rename(Time_stamp = datetime)


#蓄熱槽のデータで日付だけの列を作成し18時と6時のデータを抽出
data_hs <- data_hs %>%
  mutate(Date = as.Date(with_tz(Time_stamp, tz=tz_tokyo)))%>%
  mutate (Hours = as_hms(Time_stamp))%>%
  filter (Hours <= as_hms("6:00:00") | Hours >= as_hms("18:00:00"))

#-----気温と蓄熱槽のデータをジョイン----
df_all <-
  inner_join(data_g,data_hs, by="Time_stamp") %>%
  select (Time_stamp,  Date.y, Hours.x, Air_T_Cont_new, RZT_W_HP, HST_W_HP, RZT_A_HP, HST_A_HP) %>%
  rename(Date =Date.y, Hours =Hours.x, Air_T = Air_T_Cont_new)

# df_long <- df_all %>%
#   pivot_longer(
#     cols = c("Air_T", "RZT_W_HP", "HST_W_HP", "RZT_A_HP", "HST_A_HP"),
#     names_to ="Type",
#     values_to ="Value")


library(dplyr); library(purrr)

df <- df_all %>% 
  filter (Time_stamp >= from, Time_stamp <= to) 

prep_one <- function(df){
  df <- arrange(df, Time_stamp)
  dt  <- as.numeric(diff(df$Time_stamp), units = "secs")
  y   <- diff(df$RZT_W_HP) / dt
  X1  <- head(df$Air_T, -1) - head(df$RZT_W_HP, -1)
  X2  <- head(df$HST_W_HP, -1) - head(df$RZT_W_HP, -1)
  tibble(df$Date[-1], y, X1, X2, w = dt)
}

reg_by_day <- df %>%
  group_by(Date) %>%
  group_split()%>%
  map_df(~{
    dat <- prep_one(.x)
    D <- .x$Date[1]
    if(nrow(dat) < 5) return(tibble(Date = unique(dat$Date), kA=NA, kC=NA, seA=NA, seC=NA, n=nrow(dat)))
    fit <- lm(y ~ 0 + X1 + X2, data = dat, weights = dat$w)
    co  <- coef(summary(fit))
    tibble(
      Date = D,
      kA  = co["X1","Estimate"],
      kC  = co["X2","Estimate"],
      seA = co["X1","Std. Error"],
      seC = co["X2","Std. Error"],
      n   = nrow(dat)
    )
  }
)

reg_by_day <- reg_by_day %>% mutate(rel_seA = seA / kA, rel_seC = seC / kC)


#シミュレーション----
library(dplyr)
library(purrr)
library(tidyr)

# --- 安全版: 1日分を前進シミュレーション ---
# TA = Air_T, TC = HST_W_HP, TB = RZT_W_HP
simulate_one_day <- function(df_day, kA, kC, beta0 = 0){
  df_day <- df_day %>% arrange(Time_stamp)
  n <- nrow(df_day)
  if(n < 2 || is.na(kA) || is.na(kC)){
    return(df_day %>%
             transmute(Date, Time_stamp,
                       TB_obs = RZT_W_HP,
                       TB_sim = RZT_W_HP,   # 観測をそのまま返す
                       resid  = NA_real_))
  }
  
  dt <- as.numeric(diff(df_day$Time_stamp), units = "secs")
  # 0秒や負のdt（重複・逆順）があれば除去
  keep <- dt > 0 & is.finite(dt)
  if(any(!keep)){
    df_day <- df_day[c(TRUE, keep), ]
    dt     <- dt[keep]
    n      <- nrow(df_day)
    if(n < 2){
      return(df_day %>% transmute(Date, Time_stamp,
                                  TB_obs = RZT_W_HP,
                                  TB_sim = RZT_W_HP,
                                  resid  = NA_real_))
    }
  }
  
  TB <- numeric(n)
  TB[1] <- df_day$RZT_W_HP[1]  # 初期値はその日の観測の先頭
  for(t in 1:(n-1)){
    dTBdt   <- beta0 + kA*(df_day$Air_T[t]     - TB[t]) +
      kC*(df_day$HST_W_HP[t] - TB[t])
    TB[t+1] <- TB[t] + dt[t] * dTBdt
  }
  
  tibble(
    Date       = df_day$Date,
    Time_stamp = df_day$Time_stamp,
    TB_obs     = df_day$RZT_W_HP,
    TB_sim     = TB,
    resid      = TB_obs - TB_sim
  )
}

# --- 日別に係数を当てはめて一括シミュレーション ---
# df: 複数日をつないだ観測データ（Time_stamp, Date, Air_T, HST_W_HP, RZT_W_HP）
# reg_by_day: 日別の係数表（Date, kA, kC など）
simulate_by_day <- function(df, reg_by_day, beta0 = 0){
  # 係数を日付で付与
  df2 <- df %>%
    mutate(Date = as.Date(Date)) %>%
    left_join(reg_by_day %>% select(Date, kA, kC) %>% mutate(Date = as.Date(Date)),
              by = "Date")
  
  # 日ごとに前進シミュレーション
  df2 %>%
    group_by(Date) %>%
    group_modify(~{
      kA_i <- unique(.x$kA)[1]
      kC_i <- unique(.x$kC)[1]
      simulate_one_day(.x, kA = kA_i, kC = kC_i, beta0 = beta0)
    }) %>%
    ungroup()
}

# --- 指標を計算（R² は観測TBに対する決定係数） ---
metrics_by_day <- function(simdf){
  simdf %>%
    group_by(Date) %>%
    summarize(
      n      = sum(!is.na(resid)),
      MAE    = mean(abs(resid), na.rm = TRUE),
      RMSE   = sqrt(mean(resid^2, na.rm = TRUE)),
      R2     = {
        y <- TB_obs
        yhat <- TB_sim
        1 - sum((y - yhat)^2, na.rm = TRUE) /
          sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
      },
      .groups = "drop"
    )
}

# =========================
# 使い方
# =========================

# 1) （参考）prep_oneの arrange を修正しておくと安全
#    arrange(df, Time_stamp) にしてください。timestamp() は誤りです。
# prep_one <- function(df){
#   df <- arrange(df, Time_stamp)
#   dt  <- as.numeric(diff(df$Time_stamp), units = "secs")
#   y   <- diff(df$RZT_W_HP) / dt
#   X1  <- head(df$Air_T, -1)   - head(df$RZT_W_HP, -1)
#   X2  <- head(df$HST_W_HP, -1)- head(df$RZT_W_HP, -1)
#   tibble(Date = df$Date[-1], y, X1, X2, w = dt)
# }

# 2) すでに作成済みの reg_by_day（Date, kA, kC, ...）を使用して日別シミュレーション
sim_df   <- simulate_by_day(df, reg_by_day, beta0 = 0)

# 3) 日別の精度指標
by_day   <- metrics_by_day(sim_df)

# 4) 全体指標（任意）
overall <- with(sim_df, {
  ok   <- is.finite(resid)
  mae  <- mean(abs(resid[ok]))
  rmse <- sqrt(mean(resid[ok]^2))
  r2   <- 1 - sum((TB_obs[ok] - TB_sim[ok])^2) /
    sum((TB_obs[ok] - mean(TB_obs[ok]))^2)
  c(MAE = mae, RMSE = rmse, R2 = r2)
})

# 5) 例：大きくズレた日を確認
worst <- by_day %>% arrange(desc(RMSE)) %>% slice(1:5)
worst




#---------
library(lme4)
reg_all <- df_all %>%
  group_by(Date) %>%
  group_split()
map_df(prep_one)


#-回帰係数----------
df <- df_all %>% 
  filter (Time_stamp >= from, Time_stamp <= to) %>%
  arrange(Time_stamp) 

# time, Air_T, TB, HST_W_HP があるとする
dt <- as.numeric(diff(df$Time_stamp), units="secs")

dRZT_W_HP <- diff(df$RZT_W_HP) / dt     # 中心差分にするとさらに良い

X1 <- head(df$Air_T, -1) - head(df$RZT_W_HP, -1)
X2 <- head(df$HST_W_HP, -1) - head(df$RZT_W_HP, -1)
fit <- lm(dRZT_W_HP ~ 0 + X1 + X2)  # 切片なし

kA <- coef(fit)[["X1"]]; kC <- coef(fit)[["X2"]]

# 予測（新しい Air_T_new, HST_W_HP_new と初期 RZT_W_HP0）---------
predict_RZT <- function(Air_T, HST, RZT0, dt, kA, kC){
  RZT<- numeric(length(Air_T)); RZT[1] <- RZT0
  for(t in 1:(length(Air_T)-1)){
    dRZTdt <- kA*(Air_T[t]-RZT[t]) + kC*(HST[t]-RZT[t])
    RZT[t+1] <- RZT[t] + dt[t]*dRZTdt
  }
  RZT
}


dt_vec <- as.numeric(diff(df$Time_stamp), units = "secs")

# 関数は長さ n-1 の dt を期待するので、そのまま渡せます
TB0 <-  df$RZT_W_HP[nrow(df)]     # 学習区間の最後の観測 TB を初期値にする例
kA  <-  coef(fit)["X1"]; kC <- coef(fit)["X2"]  # 以前の回帰結果を利用

TB_pred <- predict_RZT(
  Air_T = df$Air_T,
  HST = df$HST_W_HP,
  RZT0 = TB0,
  dt  = dt_vec,
  kA  = kA,
  kC  = kC
)


df_res <- df %>%
  select (Time_stamp, Date, Air_T, RZT_W_HP, HST_W_HP) %>%
  mutate (Est_RZT = round(TB_pred,1))

df_long <- sim_df%>%
  pivot_longer(
    cols = c("TB_obs",
             "TB_sim"),
    names_to ="Type",
    values_to ="Value")

select <- c("TB_obs",
            "TB_sim")

col_map <- c(TB_obs = "red",
             TB_sim = "blue") 

df_plot <- df_long %>%
  select(-resid) %>%
  filter(Time_stamp >= from, Time_stamp <= to) %>%
  filter(Type %in% select, !is.na(Value)) %>%         # ← ここで対象系列だけ残す
  mutate(Type = factor(Type, levels = select))        # ← 凡例順も固定

p <- ggplot(df_plot, mapping = aes(x = Time_stamp, y = Value, color = Type, group = interaction(Date, Type, drop = TRUE)))+
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

