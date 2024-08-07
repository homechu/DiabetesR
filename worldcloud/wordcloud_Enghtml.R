install.packages("rvest")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("tmcn")

library("rvest")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("tmcn")

source.page <- read_html("https://www.emc.com/leadership/digital-universe/2014iview/executive-summary.htm")

source.content<- html_nodes(source.page, xpath = '//*[@id="content"]/div')

content<-html_text(source.content)

content

content.dtm <- Corpus(VectorSource(content))

inspect(content.dtm)

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ",x))

content.dtm<-tm_map(content.dtm , toSpace,"/")
content.dtm<-tm_map(content.dtm , toSpace,"@")
content.dtm<-tm_map(content.dtm , toSpace,"\\|")


content.dtm<-tm_map(content.dtm , content_transformer(tolower))


content.dtm <- tm_map(content.dtm, removePunctuation) # 移除標點符號
content.dtm <- tm_map(content.dtm, stripWhitespace)  # 移除額外的"空白"
content.dtm <- tm_map(content.dtm, removeNumbers)     # 移除數字
content.dtm <- tm_map(content.dtm, removeWords, stopwords("english")) # 移除英文虛字



mystopWords <- c(stopwords("english"), "the ", "and","new","will")  #移除英文虛字以及特定的字


content.dtm <- tm_map(content.dtm, removeWords, mystopWords) # 移除英文虛字

findAssocs(dtm, terms = "universe", corlimit = 0.3)
findAssocs(dtm, terms = "digital", corlimit = 0.3)
findAssocs(tdm,"data", corlimit = 0.3)


tdm <- TermDocumentMatrix(content.dtm)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing= TRUE)
d <- data.frame(word= names(v), freq = v)
head(d,10)

set.seed(1000)

wordcloud(word = d$word, freq = d$freq, min.freq =1,
          max.words = 50, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))
