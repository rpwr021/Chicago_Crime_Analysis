install.packages(c("dplyr","tm","SnowballC","wordcloud","lsa", "dplyr", "ggmap","RColorBrewer", "topicmodels"))

require(dplyr)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
cdf2 <- as.data.frame(read.csv("/home/raz/Projects/Untitled Folder/crimeReports.csv", sep = '|', header = TRUE))

cdf <- cdf2[-c(1,2,4)]
View(cdf)

#news <- Corpus(DirSource(sources, recursive=TRUE),readerControl = list(reader=readPlain))

news <- Corpus(DataframeSource(cdf))
inspect(news)

#news <- Corpus(VectorSource(news))
remq <- c("said", "Organization","writes","From", "lines"," NNTP-Posting-Host", "article")

remq <- c("said","year")
news <- tm_map(news, removeWords, remq, lazy=TRUE)
news <- tm_map(news, tolower) ## Convert to Lower Case
news <- tm_map(news, removeWords, stopwords("english"))
news <- tm_map(news, removePunctuation)
#news <- tm_map(news, removeNumbers) ## Remove Numbers
news <- tm_map(news, stripWhitespace) ## Eliminate Extra White Spaces
news <- tm_map(news , PlainTextDocument)

wordcloud(news, scale = c(3, 0.5), max.words = 50, min.freq = 5, random.order = , 
          rot.per = 0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))

news <- tm_map(news, stemDocument) ## Stemming#news <- tm_map(news , removeURL)

wordcloud(news, scale = c(3, 0.5), max.words = 50, min.freq = 5, random.order = , 
          rot.per = 0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))

dtm <- DocumentTermMatrix(news, control=list(wordLengths=c(4,Inf)))
tdm <- TermDocumentMatrix(news, control=list(wordLengths=c( 4 ,Inf))) #Term
rownames(dtm) <- cdf2$victim_name
View(news)


rownames(dtm) <- cdf2$victim_name
colnames(tdm) <- cdf2$victim_name
##########################################
dtm_tfxidf2<- weightTfIdf(dtm)
dtm_tfxidf2 <- removeSparseTerms(dtm_tfxidf2, 0.98)

m <- as.matrix(dtm_tfxidf2)
#rownames(m) <- 1:nrow(m)
rownames(m)
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)
cl<- kmeans(na.exclude(m_norm), 5)
cl$centers
plot(prcomp(na.exclude(m_norm))$x, col=cl$cl) #PCA
#################################

#distance matrix
# 2. MDS with raw term-document matrix compute distance matrix
td.mat <- as.matrix(TermDocumentMatrix(news))
#td.mat <- removeSparseTerms(dtm, 0.99)
dist.mat <- dist(t(as.matrix(td.mat)))
dist.mat  # check distance matrix
#LSA
# 3. MDS with LSA
require(lsa)

td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat)  # weighting
lsaSpace <- lsa(td.mat.lsa)  # create LSA space
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace)))  # compute distance matrix

dist.mat.lsa # check distance matrix


#sparse <- removeSparseTerms(dtm, 0.98) 
words <- as.data.frame(as.matrix(dtm)) 
wordSum <- colSums(as.matrix(dtm))
#tdm <- removeSparseTerms(tdm, 0.98) 
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
#d
plot(head(d, 150), colors = brewer.pal(8, "Dark2"))

tm_tfxidf2<- weightTfIdf(dtm)
dtm_tfxidf2 <- removeSparseTerms(dtm_tfxidf2, 0.99)
freq <- data.frame(sort(colSums(as.matrix(dtm_tfxidf2)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))
#######################################
library(tm)
library(ggplot2)
library(lsa)
set.seed( 123 )
library("NbClust")

tdm1 <- removeSparseTerms(tdm, .99)
dtm1 <- removeSparseTerms(dtm, .99)

myMatrix <- lw_logtf(tdm1) * gw_idf(tdm1)
myLSAspace = lsa(dtm_tfxidf2, dims=dimcalc_share())
as.textmatrix(myLSAspace)
# clean up
unlink(td, recursive=TRUE)

library(tm)
library(ggplot2)
library(SnowballC)

ttt<- removeSparseTerms(ydtm,0.70)

res <- NbClust(tdm, distance = "euclidean", min.nc = 2, max.nc = 6, method = "kmeans", index = "duda")
res$Best.nc


require("lsa")
nws_svd <- svd(tdm)
su <- sort.list(abs(nws_svd$u[, 1]), decreasing = TRUE)
dim(nws_svd$v)
nws_svd$d*nws_svd$v
dtm$dimnames$Terms[head(su, 8)]

sv <- sort.list(abs(nws_svd$v[, 1]), decreasing = TRUE)
dtm$dimnames$Terms[head(sv, 100)]




require('topicmodels')
k <- 5
#dtmss<- removeSparseTerms(ydtm, .)
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtss   <- dtm[rowTotals> 0, ] 
ldaOut <-LDA(dtss,4,control = list(alpha = 0.15), method="VEM")
?LDA

get_topics(ldaOut)
get_terms(ldaOut,k=8)

plot(ldaOut)
