#install.packages("tm")
#install.packages("SnowballC")
#install.packages("ggplot2")
#install.packages("wordcloud")
getwd()
setwd("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT")
wd<-"C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining texts"
dir(wd)
library(tm)
docs <- Corpus(DirSource(wd))
docs
writeLines(as.character(docs[[1]]))
getTransformations()
docs <- tm_map(docs,removePunctuation)
docs <- tm_map(docs, removeNumbers)
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))

for (j in seq(docs)) {
  docs[[j]] <- gsub("/", "", docs[[j]])
  docs[[j]] <- gsub("@", "", docs[[j]])
  docs[[j]] <- gsub("–", "", docs[[j]])
  docs[[j]] <- gsub("’", "", docs[[j]])
  docs[[j]] <- gsub("“", "", docs[[j]])
  docs[[j]] <- gsub("…", "", docs[[j]])
  docs[[j]] <- gsub("‘", "", docs[[j]])
  docs[[j]] <- gsub(")", "", docs[[j]])
  docs[[j]] <- gsub("”", "", docs[[j]])
  docs[[j]] <- gsub("?", "", docs[[j]])
  docs[[j]] <- gsub("#", "", docs[[j]])
}
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))

docs <- tm_map(docs, tolower) 
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))

length(stopwords("english")) 
stopwords("english")

docs <- tm_map(docs, removeWords, stopwords("English"))
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))

StW<-read.table("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\stopwords.txt")
StW

StWW<-as.character(StW$V1)
StWW

docs <- tm_map(docs, removeWords, StWW)
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))

docs <- tm_map(docs, stripWhitespace)
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))

stemDocument("modelling", language = "english") 
stemDocument("modeller", language = "english") 
stemDocument("models", language = "english")
writeLines(as.character(docs[[1]]))

dtm <- DocumentTermMatrix(docs)
dtm
inspect(dtm[1:42, 140:145])
tdm <- t(dtm) 
tdm <- TermDocumentMatrix(docs) 
tdm 
inspect(tdm[170:175,37:42])

filenames <- list.files(setwd("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining texts"),pattern="*.txt")
filenames <-c(filenames)
rownames(dtmr)<-filenames

#ograniczenia dla macierzy 

dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(3, 20),bounds = list(global = c(2,Inf))))
dtmr1 = removeSparseTerms(dtmr, 0.85)
doc_length <- as.data.frame(rowSums(as.matrix(dtmr1)))
doc_length
max_length<-max(doc_length)
max_length
min_length<-min(doc_length)
min_length
aver_length<-mean(rowSums(as.matrix(dtmr1)))
aver_length

#create a normalized dtm which eliminates the difference in document length
nn<-rowSums(as.matrix(dtm))
nn
dtm_Norm<-dtm/nn
dtmr
dtmr1

m0 <- as.matrix(dtm)
write.csv(m0, file="C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining matrix\\DocumentTermMatrix.csv")
m1<-as.data.frame(as.matrix(dtm_Norm))
write.csv(m1, file="C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining matrix\\DocumentTermMatrixNorm.csv")
m2 <- as.matrix(dtmr)
write.csv(m2, file="C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining matrix\\DocumentTermMatrix_1.csv")
m3 <- as.matrix(dtmr1)
write.csv(m3, file="C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining matrix\\SparseDocumentTermMatrix.csv")

freqr <- colSums(as.matrix(dtmr1))
length(freqr)
freq <- sort(freqr, decreasing=TRUE)
head(freq, 6)

tail(freq, 14)

#relationships between terms - correlations 

findFreqTerms(dtmr1,lowfreq=15)
findAssocs(dtmr1,"matching",0.6)

#histogram

freqr <- colSums(as.matrix(dtmr1))
length(freqr)
freq <- sort(freqr, decreasing=TRUE)
mk<-min(head(freq, 30))
mk
wf=data.frame(word=names(freq),freq=freq)
library(ggplot2)
# Full Zipf's law
dev.new(width = 150, height = 100, unit = "px") #could be useful
p <- ggplot(subset(wf, freq>1), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
#Zipfs law with minimal frequency = MK
dev.new(width = 200, height = 200, unit = "px") #could be useful
p <- ggplot(subset(wf, freq>17), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#wordcloud 

library(wordcloud)

dev.new(width = 250, height = 250, unit = "px") #could be useful
set.seed(142) 
dark2 <- brewer.pal(6, "Dark2") 
wordcloud(names(freq),freq, min.freq=70)
set.seed(142) 
dev.new(width = 250, height = 250, unit = "px") #could be useful
wordcloud(names(freq), freq, max.words=100)
wordcloud(names(freq), freq, min.freq=70,colors=brewer.pal(6, "Dark2"))

set.seed(142) 
dev.new(width = 200, height = 200, unit = "px") #could be useful
dark2 <- brewer.pal(6, "Dark2") 
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)

#bigram

docs_1 <- VCorpus(DirSource(wd))
docs_1
docs_1<- tm_map(docs_1,removePunctuation)
docs_1<- tm_map(docs_1, removeNumbers) 
for (j in seq(docs_1)) { 
  docs_1 [[j]] <- gsub("/", "", docs_1[[j]]) 
  docs_1 [[j]] <- gsub("@", "", docs_1[[j]]) 
  docs_1 [[j]] <- gsub("–", "", docs_1[[j]]) 
  docs_1 [[j]] <- gsub("’", "", docs_1[[j]]) 
  docs_1 [[j]] <- gsub("“", " ", docs_1[[j]]) 
  docs_1 [[j]] <- gsub("…", "", docs_1[[j]])
  docs_1 [[j]] <- gsub("‘", "", docs_1[[j]]) 
  docs_1 [[j]] <- gsub(")", "", docs_1[[j]])
  docs_1 [[j]] <- gsub("”", "", docs_1[[j]])
  docs_1 [[j]] <- gsub("?", "", docs_1[[j]])
  docs_1 [[j]] <- gsub("#", "", docs_1[[j]])
} 
docs_1<- tm_map(docs_1, tolower) 
docs_1<- tm_map(docs_1, removeWords, stopwords("English")) 
StW<-read.table("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\stopWordsbigram.txt") 
StWW<-as.character(StW$V1) 
StWW
docs_1<- tm_map(docs_1, removeWords, StWW) 
docs_1<- tm_map(docs_1, stripWhitespace) 
for (j in seq(docs_1)) { 
  docs_1[[j]]<-stemDocument(docs_1[[j]], language = "english")} 
docs_1<- tm_map(docs_1, PlainTextDocument) 

#building bigrams based DTM MAtrix

NgramTokenizer = function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}
dtm_n <- DocumentTermMatrix(docs_1, control = list(tokenize = NgramTokenizer))
dtm_n

filenames <- list.files(getwd(),pattern="*.txt")
filenames <-c(filenames)
rownames(dtm_n)<-filenames

#create a normalized dtm which eliminates the difference in document length
dtmrb <-DocumentTermMatrix(docs_1, control=list(tokenize = NgramTokenizer, wordLengths=c(3, 20),bounds = list(global = c(2,Inf))))
dtmr1b = removeSparseTerms(dtmrb, 0.95)
nnb<-rowSums(as.matrix(dtm_n))
nnb
dtm_Normb<-dtm_n/nnb
dtmrb
dtmr1b

m0b <- as.matrix(dtm_n)
write.csv(m0b, file="C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining matrix\\DocumentTermMatrixBI.csv")
m1b <-as.data.frame(as.matrix(dtm_Normb))
write.csv(m1b, file="C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining matrix\\DocumentTermMatrixNormBI.csv")
m2b <- as.matrix(dtmrb)
write.csv(m2b, file="C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining matrix\\DocumentTermMatrix_1BI.csv")
m3b <- as.matrix(dtmr1b)
write.csv(m3b, file="C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining matrix\\SparseDocumentTermMatrixBI.csv")


#histogram bigram

freq_n <- sort(colSums(as.matrix(dtmr1b)), decreasing=TRUE)
head(freq_n, 6)
mk<-min(head(freq_n, 10))
tail(freq_n, 10) 
m<-as.matrix(dtmr1b)
write.csv(m, file="C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining matrix bigram\\_DocumentTermMatrixBI.csv")
#___________Building the Histogtram (zipf’s law)___________________
wf=data.frame(word=names(freq_n),freq=freq_n) 
wf 
p <- ggplot(subset(wf, freq>=mk), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")+ ggtitle("Histogram of Bigrams for Opinions") +labs(x="Bi
-grams",y="Frequency")
p <- p + theme(axis.text.x=element_text(angle=90, hjust=1, size=16))
p

set.seed(142) 
#dev.new(width = 200, height = 200, unit = "px") #could be useful
dark2 <- brewer.pal(6, "Dark2") 
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)

#install.packages ("cluster")
#install.packages ("fpc")
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(cluster) 
library(fpc)
setwd("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT")

MyData <-read.csv("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining matrix\\DocumentTermMatrix.csv",
                  header = TRUE, #are there column names in 1st row?
                  sep = ",", #what separates rows?
                  strip.white = TRUE, #strip out extra white space in strings.
                  fill = TRUE, #fill in rows that have unequal numbers of columns
                  comment.char = "#", #character used for comments that should not be read in
                  stringsAsFactors = FALSE #Another control for deciding whether characters should be converted to factor
)


dtm1 = as.data.frame.matrix(MyData)
dtm1 [1:42,1:42]
dtm<-dtm1[,-1]
filenames <- list.files(setwd("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining texts"),pattern="*.txt")
filenames <-c(filenames)
filenames
rownames(dtm)
rownames(dtm) <-filenames
dtm [1:22,1:20]                        

freq <- sort(colSums(dtm), decreasing=TRUE)
freq
freq1 <- sort(rowSums(dtm), decreasing=TRUE)
freq1


wf=data.frame(word=names(freq),freq=freq)
wf
p <- ggplot(subset(wf, freq>17), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

tdm<- t(dtm) # t(dtm) – transpose matrix DTM into TDM
tf <- as.matrix(tdm) 
idf <- log(ncol(tf) / (rowSums(tf != 0)))
tf[170:175,1:5]
idf[280:288]
idf_sort <- sort(idf, decreasing=FALSE)
head(idf_sort, 15)
tail(idf_sort, 15)
# building tf-idf
idf1 <- diag(idf)
tf_idf <- crossprod(tf, idf1)
colnames(tf_idf) <- rownames(tf)
write.csv(as.matrix(tf_idf),file="C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\TFIDF.csv")
tf_idf_t<-t(tf_idf) #transposed matrix tf_idf
tf_idf_t [355:365,1:3]

freq <- colSums(as.matrix(tf_idf), na.rm = FALSE)
dev.new(width = 100, height = 100, unit = "px") # if you need
set.seed(42)
wordcloud(names(freq),freq, max.words=50)
# Second view of wordcloud
dev.new(width = 130, height = 130, unit = "px") # if you need
set.seed(142) 
wordcloud(words = names(freq), freq = freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


dtm[11:12,11:13] 

# if you do not have the Filenames, do the following:
filenames <- list.files(setwd("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining texts"),pattern="*.txt")
filenames <-c(filenames)
filenames
rownames(dtm)
rownames(dtm)<-filenames 


d1 <- dist(dtm, method="euclidian")
# make the clustering
fit <- hclust(d=d1, method="complete")min
fit
plot.new()
plot(fit, hang=-1, cex=0.5)
# for a receiving the different dendrograms view ry substituting: method="ward.D" and any other form list above:

groups <- cutree(fit, k=7) # "k" defines the number of clusters you are using 
rect.hclust(fit, k=7, border="red") # draw dendogram with red borders around the 4 clusters


###clustering tf-idf

filenames <- list.files(setwd("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining texts"),pattern="*.txt")
filenames <-c(filenames)
filenames
rownames(tf_idf)
rownames(tf_idf)<-filenames 

tf_idf<-as.DocumentTermMatrix(tf_idf,weighting = weightTf)
tf_idf
d1 <- dist(tf_idf, method="euclidian")
fit1 <- hclust(d=d1, method="ward.D")
fit1
plot.new()
plot(fit1, hang=-1, cex=0.5)

groups <- cutree(fit1, k=14)
rect.hclust(fit1, k=14, border="red")


# remove the sparsity of the matrix tf_idf clustering
filenames <- list.files(setwd("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining texts"),pattern="*.txt")
filenames <-c(filenames)
filenames
rownames(tf_idf_s)
rownames(tf_idf_s)<-filenames 
tf_idf_s<-removeSparseTerms(tf_idf, 0.7)
tf_idf_s
tf_idf_t_s<-t(tf_idf_s) #transposed matrix tf_idf
tf_idf_t_s 

d1 <- dist(tdm_s, method="euclidian")
fit1 <- hclust(d=d1, method="ward.D")
fit1
plot.new()
plot(fit1, hang=-1, cex=0.5)
groups <- cutree(fit1, k=7)
rect.hclust(fit1, k=7, border="red")


dplyr::mutate(tibble)

help(rownames)

#  Perform the TERMS clustering based on  Perform the TERMS clustering based on TDM, tfidf, tfidfs
filenames <- list.files(setwd("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining texts"),pattern="*.txt")
filenames <-c(filenames)
filenames
rownames(tdm)
rownames(tdm)<-filenames 
tdm <-as.TermDocumentMatrix(tdm,weighting = weightTf)
tdm
tdm_s <-removeSparseTerms(tdm, 0.75)
tdm_s

###
tdm_t= t(tf_idf)
tdm_s_t= t(tf_idf_s)
d1 <- dist(tf_idf_t, method="euclidian")
fit1 <- hclust(d=d1, method="complete")
fit1
plot.new()
plot(fit1, hang=-1, cex=1)
groups <- cutree(fit1, k=7)
rect.hclust(fit1, k=7, border="red")
dim(tdm)

#clusplot
#K_MEANS

library(fpc)
#transform the format of dtm for possibility to do the RemoveSparseTerms
dtm <-as.DocumentTermMatrix(dtm,weighting = weightTf)
dtmr<-removeSparseTerms(dtm, 0.85)
dtmr
d <- dist(dtmr, method="euclidian")
kfit <- kmeans(d, 6)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0, cex.txt = 0.4)


#Perform the DOCUMENTS clustering based on DTM matrix


dtm <-as.DocumentTermMatrix(dtm,weighting = weightTf)
dtm
d <- dist(dtm, method="euclidian")
kfit <- kmeans(d, 8)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0, cex.txt = 0.4)



#Perform the DOCUMENTS clustering based on TF-IDF Matrix

filenames <- list.files(setwd("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining texts"),pattern="*.txt")
filenames <-c(filenames)
filenames
rownames(tf_idf)
rownames(tf_idf)<-filenames 

tf_idf <-as.DocumentTermMatrix (tf_idf,weighting = weightTf)
tf_idf
d <- dist(tf_idf, method="euclidian")
kfit <- kmeans(d, 7)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0, cex.txt = 0.4)


#Perform the DOCUMENTS clustering based on TF-IDF Matrix with decreased sparsity 

tf_idf_s= removeSparseTerms(tf_idf, 0.75)
tf_idf_s <-as.DocumentTermMatrix (tf_idf_s,weighting = weightTf)
tf_idf_s
d <- dist(tf_idf_s, method="euclidian")
kfit <- kmeans(d, 4)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0, cex.txt = 0.5)

#term clustering with k-means based on tdm_s
filenames <- list.files(setwd("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining texts"),pattern="*.txt")
filenames <-c(filenames)
filenames
rownames(tdm)
rownames(tdm)<-filenames 
tdm <-as.TermDocumentMatrix(tdm,weighting = weightTf)
tdm_s= removeSparseTerms(tdm, 0.70)
dm_t= t(tf_idf)
tdm_s_t= t(tf_idf_s)
tdm_s_t
inspect(tdm_s_t[1:5,1:5])
d <- dist(tf_idf_t_s , method="euclidian")
kfit <- kmeans(d, 9)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0, cex.txt = 0.5)

#based on tf-idf




#__________Elbow Method________check the optimal number of clusters__________________
library(tidyverse) # data manipulation
library(cluster)# clustering algorithms
#install.packages("factoextra")
library(factoextra) # clustering algorithms & visualization
library(purrr) 
set.seed(123)
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(tf_idf_t_s, k, nstart = 10 )$tot.withinss
}
# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15
# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)
wss_values
# optimal number of clusters should appear to be the bend in the knee (or elbow)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


# _________Gap Statistic Method_________________________
set.seed(123)
gap_stat <- clusGap(tf_idf_t_s, FUN = kmeans, nstart = 25,
                    K.max = 15, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)


####Perform the clustering####

set.seed(123)
# for reproducibility
km.res <- kmeans(tf_idf_s, 4, nstart = 25)
km.res

# Visualize 
fviz_cluster(km.res, data = dtm, palette = "jco", ggtheme = theme_minimal(), labelsize=5)

#install.packages("igraph")
#install.packages("topicmodels")
library("topicmodels")
library("igraph")

setwd("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT")

MyData <-read.csv("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining matrix\\DocumentTermMatrix.csv",
                  header = TRUE, #are there column names in 1st row?
                  sep = ",", #what separates rows?
                  strip.white = TRUE, #strip out extra white space in strings.
                  fill = TRUE, #fill in rows that have unequal numbers of columns
                  comment.char = "#", #character used for comments that should not be read in
                  stringsAsFactors = FALSE #Another control for deciding whether characters should be converted to factor
)


dtm1 = as.data.frame.matrix(MyData)
dtm1 [1:45,1:45]
dtm<-dtm1[,-1]
filenames <- list.files(setwd("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining texts"),pattern="*.txt")
filenames <-c(filenames)
filenames
rownames(dtm)
rownames(dtm) <-filenames
dtm [1:22,1:20]  


mm_s = as.matrix(dtm)
#mm<-as.matrix(mm_s[1:10,])
mm<-as.matrix(mm_s) # for using all documents
#function cosineSim compute cosine similarity between document vectors
#converting to distance matrix sets diagonal elements to 0
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
#compute cosine similarity between document vectors
cs <- cosineSim(mm)
cs

#filenames2
dtm1 = as.data.frame.matrix(MyData)
dtm1 [1:45,1:45]
dtm<-dtm1[,-1]
filenames <- list.files(setwd("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining texts"),pattern="*.txt")
filenames <-c(filenames)
filenames
rownames(dtm)
rownames(dtm) <-filenames
dtm [1:22,1:20]  


write.csv(as.matrix(cs),file="C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining matrix\\DocumentCosine.csv")
#create the adjacency matrix
min_cos<-0.2
cs[cs < min_cos] <- 0
cs <- round(cs,3)
#save adjacency matrix to *.csv file
write.csv(as.matrix(cs),file="C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining matrix\\DocumentAdjacencyMatrix.csv")
cs



#adjacency matrix
dat<-read.csv("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining matrix\\DocumentAdjacencyMatrix.csv", 
              header = TRUE, 
              sep = ",", 
              colClasses = NA, 
              na.string = "NA", 
              skip = 0, 
              strip.white = TRUE, 
              fill = TRUE, 
              comment.char = "#", 
              stringsAsFactors = FALSE 
)
dat
mm1 = as.data.frame.matrix(dat)
mm1=mm1[,-1]
mm1

filenames <- list.files(setwd("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining texts"),pattern="*.txt")
#filenames <-c(filenames[1:5])
filenames <-c(filenames) # for using all documents
filenames
#converting mm1 into matrix format
rownames(mm1)<-filenames
cs<-as.matrix(mm1)
cs

#initializing Igraph package
library(igraph)
#Creating undirected weighted graph 
g=graph.adjacency(cs,mode="undirected",weighted=TRUE)
g
#Checking the undirected weighted graph attributes 
list.vertex.attributes(g)
list.edge.attributes(g)
V(g)$name
E(g)$weight

#Calculate the degree of each Vertices and assign it’s to the Vertices size feature
deg <- graph.strength(g, mode="all")
deg
V(g)$size <- deg*30

#Build the Vertices color
hc5 <- terrain.colors(5) # colors – https://www.r-bloggers.com/color-palettes-in-r/
g.max <- max(deg) 
vcolors <- 5 - round(4 *(deg / g.max)) 
vcolors <- hc5[vcolors] 
vcolors

# Build the Graph Plot using different Layouts

lay_1 <- layout.fruchterman.reingold(g)
plot.igraph(g,layout= lay_1, edge.arrow.size=0.1,edge.width=E(g)$weight*30, vertex.label=V(g)$
              name,vertex.color=vcolors,vertex.size=V(g)$size, vertex.label.cex=1)
lay_2 <- layout_in_circle
plot.igraph(g,layout= lay_2, edge.arrow.size=0.1,edge.width=E(g)$weight*30, vertex.label=V(g)$
              name,vertex.color=vcolors,vertex.size=V(g)$size,vertex.label.cex=1)
lay_3 <- layout_with_kk(g)
plot.igraph(g,layout= lay_3, edge.arrow.size=0.1,edge.width=E(g)$weight*30, vertex.label=V(g)$
              name,vertex.color=vcolors,vertex.size=V(g)$size,vertex.label.cex=1)
lay_4 <- layout_randomly(g)
plot.igraph(g,layout= lay_4, edge.arrow.size=0.1,edge.width=E(g)$weight*30, vertex.label=V(g)$
              name,vertex.color=vcolors,vertex.size=V(g)$size,vertex.label.cex=1)
lay_5 <- layout_on_sphere(g)
plot.igraph(g,layout= lay_5, edge.arrow.size=0.1,edge.width=E(g)$weight*30, vertex.label=V(g)$
              name,vertex.color=vcolors,vertex.size=V(g)$size,vertex.label.cex=1)

# http://www.kateto.net/wp-content/uploads/2016/01/NetSciX_2016_Workshop.pdf
# Algorithm 1: edge betweenness (Newman-Girvan)
ceb <- cluster_edge_betweenness(g)
plot(ceb, g)
membership(ceb)
# Algorithm 2: based on propagating labels
clp <- cluster_label_prop(g)
plot(clp , g)
membership(clp)
# Algorithm 3: based on greedy optimization of modularity
cfg <- cluster_fast_greedy(as.undirected(g))
plot(cfg, as.undirected(g))
membership(cfg)

#terms
filenames <- list.files(setwd("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining texts"),pattern="*.txt")
filenames <-c(filenames)
filenames
rownames(tdm)
rownames(tdm)<-filenames 
tdm <-as.TermDocumentMatrix(tdm,weighting = weightTf)
tdm
tdm_s <-removeSparseTerms(tdm, 0.90)
tdm_s
tdm_t= t(tf_idf)
tdm_s_t= t(tf_idf_s)


# building Term Document Matrix as a transformed Document Term Matrix
tdm <-as.TermDocumentMatrix(t(dtm),weighting = weightTf)
tdm
# transform Term Document Matrix into the matrix with sparsity is not higher then (for example) 0.2
tdm = removeSparseTerms(tdm, 0.70)
tdm 
# completing the matrix column names
filenames <- list.files(getwd(),pattern="*.txt")
filenames <-c(filenames)
filenames
colnames(tdm)<-filenames
tdm

tdm<- t(dtm) # t(dtm) – transpose matrix DTM into TDM
tf <- as.matrix(tdm) 
idf <- log(ncol(tf) / (rowSums(tf != 0)))
tf[170:175,1:5]
idf[280:288]
idf_sort <- sort(idf, decreasing=FALSE)
head(idf_sort, 15)
tail(idf_sort, 15)
# building tf-idf
idf1 <- diag(idf)
tf_idf <- crossprod(tf, idf1)
colnames(tf_idf) <- rownames(tf)
write.csv(as.matrix(tf_idf),file="C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\TFIDF.csv")
tf_idf_t<-t(tf_idf) #transposed matrix tf_idf
tf_idf_t [355:365,1:3]


mm_s = as.matrix(tf_idf_t_s)
#mm<-as.matrix(mm_s[1:10,])
mm<-as.matrix(mm_s) # for using all documents
#function cosineSim compute cosine similarity between document vectors
#converting to distance matrix sets diagonal elements to 0
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
#compute cosine similarity between document vectors
cs <- cosineSim(mm)
cs

write.csv(as.matrix(cs),file="C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining matrix\\DocumentCosine.csv")
#create the adjacency matrix
min_cos<-0.2
cs[cs < min_cos] <- 0
cs <- round(cs,3)
#save adjacency matrix to *.csv file
write.csv(as.matrix(cs),file="C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining matrix\\DocumentAdjacencyMatrix.csv")
cs

#adjacency matrix
dat<-read.csv("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining matrix\\DocumentAdjacencyMatrix.csv", 
              header = TRUE, 
              sep = ",", 
              colClasses = NA, 
              na.string = "NA", 
              skip = 0, 
              strip.white = TRUE, 
              fill = TRUE, 
              comment.char = "#", 
              stringsAsFactors = FALSE 
)
dat
mm1 = as.data.frame.matrix(dat)
mm1=mm1[,-1]
mm1

filenames <- list.files(setwd("C:\\Users\\Uzytkownik\\OneDrive\\Pulpit\\politechnika\\TEXT MINING PROJEKT\\data mining texts"),pattern="*.txt")
#filenames <-c(filenames[1:5])
filenames <-c(filenames) # for using all documents
filenames
#converting mm1 into matrix format
rownames(mm1)<-filenames
cs<-as.matrix(mm1)
cs

#initializing Igraph package
library(igraph)
#Creating undirected weighted graph 
g=graph.adjacency(cs,mode="undirected",weighted=TRUE)
g
#Checking the undirected weighted graph attributes 
list.vertex.attributes(g)
list.edge.attributes(g)
V(g)$name
E(g)$weight

#Calculate the degree of each Vertices and assign it’s to the Vertices size feature
deg <- graph.strength(g, mode="all")
deg
V(g)$size <- deg*1

#Build the Vertices color
hc5 <- terrain.colors(5) # colors – https://www.r-bloggers.com/color-palettes-in-r/
g.max <- max(deg) 
vcolors <- 5 - round(4 *(deg / g.max)) 
vcolors <- hc5[vcolors] 
vcolors

# Build the Graph Plot using different Layouts

lay_1 <- layout.fruchterman.reingold(g)
plot.igraph(g,layout= lay_1, edge.arrow.size=0.1,edge.width=E(g)$weight*2, vertex.label=V(g)$
              name,vertex.color=vcolors,vertex.size=V(g)$size, vertex.label.cex=1, node.size=0.1)

lay_2 <- layout_in_circle
plot.igraph(g,layout= lay_2, edge.arrow.size=0.1,edge.width=E(g)$weight*2, vertex.label=V(g)$
              name,vertex.color=vcolors,vertex.size=V(g)$size,vertex.label.cex=1)
lay_3 <- layout_with_kk(g)
plot.igraph(g,layout= lay_3, edge.arrow.size=0.1,edge.width=E(g)$weight*30, vertex.label=V(g)$
              name,vertex.color=vcolors,vertex.size=V(g)$size,vertex.label.cex=1)
lay_4 <- layout_randomly(g)
plot.igraph(g,layout= lay_4, edge.arrow.size=0.1,edge.width=E(g)$weight*30, vertex.label=V(g)$
              name,vertex.color=vcolors,vertex.size=V(g)$size,vertex.label.cex=1)
lay_5 <- layout_on_sphere(g)
plot.igraph(g,layout= lay_5, edge.arrow.size=0.1,edge.width=E(g)$weight*30, vertex.label=V(g)$
              name,vertex.color=vcolors,vertex.size=V(g)$size,vertex.label.cex=1)

#Algorithm 1: edge betweenness (Newman-Girvan)
ceb <- cluster_edge_betweenness(g)
plot(ceb, g)
membership(ceb)
# Algorithm 2: based on propagating labels
clp <- cluster_label_prop(g)
plot(clp , g)
membership(clp)
# Algorithm 3: based on greedy optimization of modularity
cfg <- cluster_fast_greedy(as.undirected(g))
plot(cfg, as.undirected(g))
membership(cfg)

#Check the most significant 6 terms in each topic, transform them into the matrix format and save to to file
ldaOut.terms <- as.matrix(terms(ldaOut,42))
ldaOut.terms
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

#Transform the probabilities associated with each topic assignment into the matrix format and save to the file
topicProbabilities <- as.data.frame(ldaOut@gamma)
topicProbabilities
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))

#Transform the Topics into the matrix format and save to the File
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.topics
write.csv(ldaOut.topics,file=paste("LDAGibbs",k, "DocsToTopics.csv"))

# Cosine Distance transformed from Cosine is similarity based on dtm
mm = as.matrix(dtm)
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(mm)
d1 <- 1-cs
# Replace line:
#d1 <- dist(dtm, method="euclidian")
