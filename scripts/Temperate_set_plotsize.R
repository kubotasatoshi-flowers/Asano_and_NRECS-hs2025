#前提条件
# pにggplotでデータを代入する
# p <- ggplot（）
#pの代入が終了した後に以下のスクリプトを追加する。

#出力PDFファイル名の生成
pdf_file_name <- paste("/cloud/project/figs/",Data_file,"_J-10.pdf",sep = "")  #ディレクトリと変数(Data_file)は任意に指定しています。
as.character(pdf_file_name)

# 軸の物理サイズ（cm単位）
panel_width_cm <- 8          #任意の大きさ
panel_height_cm <- 8         #任意の大きさ

#描画エリアの全体の大きさ
area_width_cm <- 13          #任意の大きさ
area_height_cm <- 13         #任意の大きさ

g <- ggplotGrob(p)

# パネルのインデックスを取得
panel_index <- which(g$layout$name == "panel")

# 対象パネルの幅・高さ（unitオブジェクト）を上書き
g$widths[g$layout[panel_index, "l"]] <- unit(panel_width_cm, "cm")
g$heights[g$layout[panel_index, "t"]] <- unit(panel_height_cm, "cm")

# PDFなどへ出力（cairo_pdfを使うと綺麗）
cairo_pdf(pdf_file_name, width = area_width_cm/2.54, height = area_height_cm/2.54,  family = "Liberation Serif")
grid.newpage()
grid.draw(g)
dev.off()