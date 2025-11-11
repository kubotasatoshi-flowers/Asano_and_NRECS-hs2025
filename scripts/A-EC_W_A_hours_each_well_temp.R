# パッケージ読み込み
library(ggplot2)
library(tidyr)
library(dplyr)
library(legendry)
library(scales)
library(crayon)


#ひな形を読み込み
source("source/Theme_Box_line.R")

data <- read.table("/cloud/project/data/outall_difference2.csv",sep=",", comment.char="#", header=T) 

names(data)[3] <- "kWh"

p <-
  ggplot(data=data, mapping=    aes(x = Hours, y = kWh, color = Type, group = Type))+
  geom_line(stat = "identity", position = "identity", size = 0.6) +
  geom_point()+

  
  scale_x_continuous(limits = c(0, 72),
    breaks = seq(0, 72, 6),
    minor_breaks = NULL
  )+


  scale_y_continuous(limits = c(0, 30),
                     breaks = seq(0, 30, 5),
                     minor_breaks = NULL,
                     name= "Electric consumption(kWh)"
                     )+
  
  theme_minimal()+
  #ひな形を指定
  Theme_Box_line(base_family = "latin-times") +
  
  #オプションのみ指定
  
  theme(
  legend.position= c(0.4,1))
  


##direct.label(p, "first.qp")
p
# # 画像ファイルとして保存
ggsave("/cloud/project/figs/EC_water_HP_well_temp.pdf", plot = p, width = 10, height = 10, units = "cm", device=cairo_pdf)

