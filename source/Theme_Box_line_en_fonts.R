#ボックスの線グラフ
Theme_Box_line <- function(){
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(
      fill = NA,
      color = "black",
      linewidth = 1
      ),
    
    panel.grid.major = element_line(
      color = "gray90",
      linetype = "solid",
      linewidth = 0.2
      ),
    
    plot.background = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.length = unit(1, "mm"),
    legend.justification = c(1,1),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, family = "FreeSerif"), 
    text = element_text(color = "black", size = 12, family = "sans", face = "plain"),           # 全体のベースフォント
    axis.title = element_text(size = 12, family = "Liberation Serif"),
    axis.text = element_text(size = 12, family = "sans", color = "black"),
    plot.title = element_text(size = 12, family = "sans", face = "bold")
  )
}