library(systemfonts)
library(extrafont)
#font_import(prompt = FALSE) #この行は一つのワークスペースで1回実行すれば良い
loadfonts()

#ワークスペースを起動したときに自動的にフォントをロードするには以下のようにすると良い。
#設定を行うワークスペースを開き，フォルダーの/Cloud/projet内に
#.Rprofileというテキストファイルを作成する。

# その中に以下のスクリプトを記入する。
# 
# if (interactive()) {
#   try(extrafont::loadfonts(quiet = TRUE))
# }

# これにより，ワークスペースを開始するたびに外部フォントがロードされ，使用可能となる。
