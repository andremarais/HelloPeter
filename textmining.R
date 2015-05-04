require(wordcloud)
require(tm)
require(topicmodels)

setwd("C:/Users/anmarais/Desktop/GitHub/HelloPeter")


miway <- readRDS("MiWay.rds")
discIns <- readRDS("DiscoveryInsure.rds")
discH <- readRDS("Discovery.rds")
mom <- readRDS("Momentum.rds")



article.tm <- Corpus(VectorSource(as.character(mom)),list(reader=readPlain))

wc <- Corpus(VectorSource(article.tm))
wc <- tm_map(wc, stripWhitespace)
wc <- tm_map(wc, content_transformer(tolower))
wc <- tm_map(wc, removePunctuation)
wc <- tm_map(wc, removeNumbers)
wc <- tm_map(wc, removeWords, c(stopwords("english"), "momentum", "call", "email","get", "will", "now", "still", "policy"))


tdm <- TermDocumentMatrix(wc,control = list(reader=readPlain))

# Corpus(DirSource(dataPath,pattern=".csv"),list(reader=readPlain))

m <- as.matrix(tdm)

v <- sort(rowSums(m), decreasing = T)

wordcloud(names(v),v,c(4,.2),2,100,random.order=F,random.color=F,rot.per=0.15,colors=brewer.pal(8,"Dark2"))





dtm <- DocumentTermMatrix(wc)

# Also create one with TfIDf weights
dtm1 <- DocumentTermMatrix(wc,control = list(weighting=weightTfIdf))

# Select terms based on TfIDf weights and find topics
m <- as.matrix(dtm1)
v <- rowSums(m)
hist(v)



# We find good peak between 3 and 8 
topics2 <- LDA(dtm[v2<= 1.2 & tm2 > 0.9,],3,method="Gibbs")
#'
#'  Let us review the topics
terms(topics2)


###


predicttopics <- posterior(topics2,dtm)

str(predicttopics)
predicttopics$topics[1,]
#'
#'
docClass <- apply(predicttopics$topics,1,function (x) {m <- max(x); which(x == m)[1]})
#'
docClass
#'
topic.text <- rep(" ",length(topics2))

for(i in 1:3)
  topic.text[i] <- paste(wc[docClass == i],collapse=" ")

topics <- Corpus(VectorSource(topic.text))
#'
topics <- tm_map(topics , stripWhitespace)
#'
topics <- tm_map(topics , removePunctuation)
#'
topics <- tm_map(topics ,removeNumbers)
#'
#'
# Create documentterm matrix for LDA
tdm <- TermDocumentMatrix(topics)

# Select terms based on TfIDf weights and find topics
mt <- as.matrix(tdm)

dim(mt)
#'
#'
comparison.cloud(mt,scale=c(3,0.5),random.order=FALSE,rot.per=0.1,colors=brewer.pal(8,"Dark2"))

