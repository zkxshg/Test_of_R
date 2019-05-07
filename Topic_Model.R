#读入数据
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)

getwd()
setwd("~/data")
#建立语料库
name <- file.path("~/data")
length(dir(name))
dir(name)
docs <- Corpus(DirSource(name))
docs

#文本转换
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument, language = "porter")

docs <- tm_map(docs, removeWords, c("applause", "can", "cant", "will", "that", "weve", "dont","wont", "youll", "youre","thats")) 

#检查维度
dtm <- DocumentTermMatrix(docs)
dim(dtm)
dtm = removeSparseTerms(dtm, 0.75)
dim(dtm)

#检查文档-词矩阵
rownames(dtm) <- c("2010","2011","2012","2013","2014","2015","2016")
inspect(dtm[1:7, 1:5])

#矩阵降序重新排列
freq = colSums(as.matrix(dtm))
ord = order(-freq) #order the frequency
freq[head(ord)]
freq[tail(ord)]

#处理未处理的频繁词
docs <- tm_map(docs, removeWords, c("span", "applaus", "div", "meta", "american", "year"))
dtm <- DocumentTermMatrix(docs)
dtm = removeSparseTerms(dtm, 0.75)
dim(dtm)
rownames(dtm) <- c("2010","2011","2012","2013","2014","2015","2016")
inspect(dtm[1:7, 1:5])
freq = colSums(as.matrix(dtm))
ord = order(-freq) 
freq[head(ord)]

#绘制词频表
head(table(freq))
tail(table(freq))
findFreqTerms(dtm, 200)
findAssocs(dtm, "job", corlimit = 0.85)

#可视化词频
wordcloud(names(freq), freq,min.freq = 70, scale = c(3, .5), colors=brewer.pal(6, "Dark2"))
wordcloud(names(freq), freq, max.words = 25)

freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
wf <- data.frame(word = names(freq), freq = freq)
wf <- wf[1:10, ]
barplot(wf$freq, names = wf$word, main = "Word Frequency",xlab = "Words", ylab = "Counts", ylim = c(0, 250))

#建立主题模型
library(topicmodels)
set.seed(123)
lda3 <- LDA(dtm, k = 3, method = "Gibbs")
topics(lda3)
set.seed(456)
terms(lda3, 25)
