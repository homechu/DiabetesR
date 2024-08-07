install.packages("tm")
library(tm)
install.packages("wordcloud")
library(wordcloud)

fileText <- readLines("C:/Users/Chu's PC/Desktop/文字雲論文/Social Media Mining with R.txt")
head(fileText,10)
docs <- Corpus(VectorSource(fileText))
head(docs)
inspect(docs)
meta(docs[[1]])

docs <- tm_map(docs, removePunctuation) # ???????I?Ÿ?
docs <- tm_map(docs, stripWhitespace)  # ?????B?~??"?ť?"
docs <- tm_map(docs, removeNumbers)     # ?????Ʀr
mystopWords <- c(stopwords("english"),"the","can","this", "and","new","will")
docs <- tm_map(docs, removeWords, mystopWords) # ?????^?????r

docs <- tm_map(docs, function(x)removeWords(x, stopwords()))

tdm <- TermDocumentMatrix(docs)

m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing= TRUE)
head(v,30)
d <- data.frame(word=names(v), freq=v)
head(d,30)



wordcloud(d$word, d$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

install.packages("wordcloud2") # ????"???r??"??
library("wordcloud2")

#shape The shape of the "cloud" to draw. Can be a keyword present. Available presents
#are ??circle?? (default), ??cardioid?? (apple or heart shape curve, the most known
#polar equation), ??diamond?? (alias of square), ??triangle-forward??, ??triangle??, ??pentagon??,
#and ??star??.

wordcloud2(as.table(v), fontFamily = 'Arial', shape = 'circle')
wordcloud2(as.table(v), fontFamily = 'Arial', shape = 'star')
wordcloud2(as.table(v), fontFamily = 'Arial', shape = 'diamond')
wordcloud2(as.table(v), fontFamily = 'Arial', shape = 'pentagon')
