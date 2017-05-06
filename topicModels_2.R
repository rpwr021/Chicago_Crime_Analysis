library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

cdf2 <- as.data.frame(read.csv("/home/raz/Projects/Untitled Folder/crimeReports.csv", sep = '|', header = TRUE))

cdf <- cdf2[-c(1,2,4)]
#View(cdf)
#news <- Corpus(DirSource(sources, recursive=TRUE),readerControl = list(reader=readPlain))

news <- Corpus(DataframeSource(cdf))
inspect(news)

remq <- c("said","year")

news <- tm_map(news, removeWords, remq, lazy=TRUE)
news <- tm_map(news, tolower) ## Convert to Lower Case
news <- tm_map(news, removeWords, stopwords("english"))
news <- tm_map(news, removePunctuation)
#news <- tm_map(news, removeNumbers) ## Remove Numbers
news <- tm_map(news, stripWhitespace) ## Eliminate Extra White Spaces
news <- tm_map(news , PlainTextDocument)

#ndocs <- length(news)
## ignore overly sparse terms (appearing in less than 1% of the documents)
#minDocFreq <- ndocs * 0.01
# ignore overly common terms (appearing in more than 80% of the documents)
#maxDocFreq <- ndocs * 0.8
#dtmx<- DocumentTermMatrix(news, control=list(bounds=list(global=c(minDocFreq, maxDocFreq)))


dtm <- DocumentTermMatrix(news, control=list(wordLengths=c(3,Inf)))
tdm <- TermDocumentMatrix(news, control=list(wordLengths=c(3,Inf))) #Term


#tdm1 <- removeSparseTerms(tdm, .93)
#dtm1 <- removeSparseTerms(dtm, .93)

freq <- colSums(as.matrix(dtm))
wordcloud(news, scale = c(3, 0.5), max.words = 50, min.freq = 5, random.order = , 
          rot.per = 0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))

table(freq)

############STEM
news <- tm_map(news, stemDocument) ## Stemming#news <- tm_map(news , removeURL)

wordcloud(news, scale = c(3, 0.5), max.words = 50, min.freq = 5, random.order = , 
          rot.per = 0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))

rownames(dtm) <- cdf2$victim_name
colnames(tdm) <- cdf2$victim_name
dim(dtm)
#tdm <- TermDocumentMatrix(news, control=list(wordLengths=c( 4 ,Inf))) #Term

m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

head(d, 30)


barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


#################################
dtm_tfxidf<- weightTfIdf(dtm)
inspect(dtm_tfxidf)
freq = data.frame(sort(colSums(as.matrix(dtm_tfxidf)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100, random.order=FALSE, rot.per=0.45, colors=brewer.pal(8, "Dark2"))

barplot(table(freq))

m <- as.matrix(dtm_tfxidf)

#rownames(m) <- 1:nrow(m)
rownames(m)

norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)
cl <- kmeans(na.exclude(m_norm), 8)
plot(table(cl$cluster)) # to check count of each cluster
cl$centers
plot(prcomp(na.exclude(m_norm))$x, col=cl$cl) #PCA
#################################

set.seed( 123 )
library("NbClust")


reduce <- function(A,dim) {
  #Calculates the SVD
  sing <- svd(A)
  #Approximate each result of SVD with the given dimension
  u<-as.matrix(sing$u[, 1:dim])
  v<-as.matrix(sing$v[, 1:dim])
  d<-as.matrix(diag(sing$d)[1:dim, 1:dim])
  #Create the new approximated matrix
  return(as.matrix(u%*%d%*%t(v),type='blue'))
}

res <- NbClust(dtm, distance = "euclidean", min.nc = 2, max.nc = 8, method = "kmeans", index = "dindex")
res$Best.nc
######################
m <- as.matrix(dtm_tfxidf)
#rownames(m) <- 1:nrow(m)
rownames(m)
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)
cl<- kmeans(m_norm, 6)
table(cl$cluster) # to check count of each cluster
#remove prcomp function to plot cluster without PCA

plot(prcomp(m_norm)$x, col=cl$cl) #PCA
inspect(news[which(cl$cluster==6)])
#########################
sing <- svd(tdm)
sv = sort.list(abs(sing$u[, 1]), decreasing = TRUE)
dtm$dimnames$Terms[head(sv, 20)]

###########
reduce <- function(A,dim) {
  #Calculates the SVD
  sing <- svd(A)
  #Approximate each result of SVD with the given dimension
  print(dim(A))
  u<-as.matrix(sing$u[, 1:dim])
  v<-as.matrix(sing$v[, 1:dim])
  d<-as.matrix(diag(sing$d)[1:dim, 1:dim])
  #Create the new approximated matrix
  return(as.matrix(u%*%d%*%t(v),type='blue'))
}

rdtm<- reduce(dtm,10)
m <- as.matrix(rdtm)

rownames(m) <- 1:nrow(m)

norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)
cl<- kmeans(m_norm, 7)
kmeans(rdtm, 7)
table(cl$cluster)

###################################################################

require('topicmodels')
k <- 5
#dtmss<- removeSparseTerms(ydtm, .)
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtss   <- dtm[rowTotals> 0, ] 
ldaOut <-LDA(dtss,3,control = list(alpha = 0.1), method="VEM")

get_topics(ldaOut)
get_terms(ldaOut,k=10)

plot(ldaOut)


