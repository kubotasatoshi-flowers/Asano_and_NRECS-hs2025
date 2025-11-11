#ボックスの線グラフ
Theme_genes_bar <- function(){
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
    #legend.direction = "horizontal",
    legend.justification = c(1, 1),
    legend.title = element_blank(),
    legend.text = element_text(size = 8, family = "IPAexMincho"), 
    text = element_text(color = "black", size = 10, family = "IPAexMincho", face = "plain"),           # 全体のベースフォント
    axis.title = element_text(size = 10, family = "IPAexMincho"),
    axis.text = element_text(size = 10, family = "Times New Roman", color = "black"),
    plot.title = element_text(size = 10, family = "IPAexMincho", face = "bold")
    
    
    # text = element_text(size = 12, family = "Times New Roman"),           # 全体のベースフォント
    # axis.title = element_text(size = 12, family = "Times New Roman"),
    # axis.text = element_text(size = 12, family = "Times New Roman"),
    # #legend.text = element_text(size = 10, family = "Times New Roman"),
    # plot.title = element_text(size = 10, family = "Times New Roman", face = "bold")
  )
}