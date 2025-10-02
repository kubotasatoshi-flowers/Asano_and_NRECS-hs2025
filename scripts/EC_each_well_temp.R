#井戸の温度を変えたときの消費電力量の違いのグラフ

library(ggplot2)
library(tidyr)
library(dplyr)
library(legendry)
library(scales)
library(crayon)
library(readr)

#ひな形を読み込み
source("source/Theme_Box_line.R")



# 読み込むフォルダを指定
path <- "/cloud/project/data/"
fn <- c("EC_dif_08-16 to 08-19.csv", #Bubbling:Off, 22°C/21°C
        "EC_dif_08-22 to 08-24.csv", #Bubbling:Off, 25°C/24°C
        "EC_dif_08-25 to 08-27.csv", #Bubbling:Off, 28°C/27°C
        "EC_dif_08-28 to 08-31.csv", #Bubbling:On, 22°C/21°C
        "EC_dif_09-01 to 09-04.csv"  #Bubbling:Off, 22°C/21°C
        )

# CSVファイル一覧を取得
files <- paste0(path,fn)

# まとめて読み込み → リストになる

df_list <- lapply(files, function(f) {
  read_csv(f, comment="#") %>%
    mutate(source_file = basename(f))   # ファイル名だけ（パスを除く）
  # mutate(source_file = f)           # フルパスを残したい場合はこちら
})


# 1つのデータフレームに結合
df_all <- bind_rows(df_list, .id = "file_id")

df_all <-  df_all %>%
  mutate(Treat = recode(Type,
                       "08-16 to 08-19" = "Bubbling:Off, 22°C/21°C",
                       "08-22 to 08-24" = "Bubbling:Off, 25°C/24°C",
                       "08-25 to 08-27" = "Bubbling:Off, 28°C/27°C",
                       "08-28 to 08-31" = "Bubbling:On, 22°C/21°C",
                       "09-01 to 09-04" = "Bubbling:Off, 22°C/21°C"
                       ))

df_all <- df_all %>%
  filter(Type!="09-01 to 09-04")
p <-
  ggplot(data = df_all,
         mapping = aes(x = Difference_time, y = Difference_EC, color = Treat, group = Treat))+
  geom_line(stat = "identity", position = "identity", size = 0.6) +
  geom_point()+
  
  scale_x_continuous(limits = c(0, 72),
                     breaks = seq(0, 72, 12),
                     minor_breaks =  seq(0, 72, 6),
                     name = "Time after beggining of treatments (hours)"
  )+
  
  scale_y_continuous(limits = c(0, 30),
                     breaks = seq(0, 30, 5),
                     minor_breaks = NULL,
                     name= "Electric consumption (kWh)"
  )+
  
  theme_minimal()+
  #ひな形を指定
  Theme_Box_line(base_family = "latin-times") +
  
  #オプションのみ指定
  theme(
    legend.direction = "vertical",
    legend.position = c(0.02, 0.95),
    legend.key.height = unit(0.4, "cm"),   # キーの高さを小さく
    legend.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(size = 0.1, color="black"),
    panel.grid.major.y = element_line(size = 0.1, color="black")
  )
p

# # 画像ファイルとして保存
ggsave("/cloud/project/figs/EC_difference_water_HP.pdf", plot = p, width = 10, height = 10, units = "cm", device=cairo_pdf)
