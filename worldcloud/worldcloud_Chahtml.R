install.packages("rvest")
install.packages("tm")
install.packages("jiebaR")
install.packages("wordcloud2")


library("rvest")
library("tm")
library("jiebaR")
library("wordcloud2")

source.page <- read_html( "https://www.emc.com/leadership/digital-universe/2014iview/executive-summary.htm" )

source.content<- html_nodes(source.page, xpath = '//*[@id="container"]')

content<-html_text(source.content)

content

mixseg = worker()
content.vec <-segment(  code = content, jiebar = mixseg)


space_tokenizer = function(x){
  unlist(strsplit(as.character(x[[ 1]]), '[[:space:]]+' ))
}
jieba_tokenizer = function(d){
  unlist(segment(d[[ 1]], mixseg))
}


CNCorpus = function(d.vec){
  doc <-VCorpus(VectorSource(d.vec))
  doc <-unlist(tm_map(doc ,jieba_tokenizer), recursive = F)
  doc <-lapply(doc , function(d)paste(d, collapse = ' '))
  
  Corpus(VectorSource(doc))
}

content.corpus = CNCorpus(list(content.vec)) # 執行 CNCorpus 副程式
content.corpus <-tm_map(content.corpus, removeWords, mystopwords)
content.corpus <-tm_map(content.corpus, removeNumbers) # 移除數字
control.list = list(wordLengths = c(2, Inf),tokenize = space_tokenizer)
content.dtm <-DocumentTermMatrix(content.corpus, control = control.list)
inspect(content.dtm)



content.corpus = CNCorpus(list(content.vec))
content.corpus <- tm_map(content.corpus, removeNumbers)

control.list = list(wordLengths = c(2, Inf), tokenize = space_tokenizer)
content.dtm <- DocumentTermMatrix(content.corpus, control = control.list)

inspect(content.dtm)

frequency <- colSums(as.matrix(content.dtm))
frequency <- sort(frequency, decreasing = TRUE)[1:100]

wordcloud2(as.table(frequency), fontFamily = '微軟雅黑', shape = 'star')

