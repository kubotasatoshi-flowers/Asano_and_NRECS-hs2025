#ボックスの線グラフ
Theme_Box_line <- function(base_family = NULL) {
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
    panel.grid.major = element_line(color = "gray90", linetype = "solid", linewidth = 0.2),
    
    plot.background = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.length = unit(1, "mm"),
    
    legend.justification = c(0, 1),
    legend.title = element_blank(),
    
    # ← familyを書かずにサイズ等だけ。base_familyを継承します
    text       = element_text(color = "black", size = 12, face = "plain", family = base_family),
    axis.title = element_text(size = 12, family = base_family),
    axis.text  = element_text(size = 10, family = base_family, color = "black"),
    legend.text= element_text(size = 10, family = base_family),
    plot.title = element_text(size = 10, family = base_family, face = "bold"),
    plot.caption = element_text(size = 11, family = base_family, face = "bold")
  )
}
