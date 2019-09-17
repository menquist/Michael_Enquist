setwd("~/Downloads")
library(pdftools)
nvsr65_05 <- pdf_text("other downloads/WY (Weyerhaeuser Company)  (10-Q) 2017-07-28.pdf")
#head(strsplit(nvsr65_05[ [  1 ] ], "\n")[ [ 1 ] ])
str(nvsr65_05)
df <- data.frame(0,nvsr65_05)
df$X0 <- 1:nrow(df)
colnames(df)[1] <- "ID"


#library(rJava)
write.csv(df,"wey-test.csv")

#x <- read.csv("basu.csv", stringsAsFactors=FALSE)

#df[,2] <- as.character(df[,2])
#str(df)

#df[,sapply(df,is.character)] <- sapply(
#  df[,sapply(df,is.character)],
#  iconv,"WINDOWS-1252","UTF-8")


#x <- read.csv("child_labour_basu.TXT")
#str(x)
#x = read.table("child_labour_basu.TXT", sep=":", header=TRUE, fill=TRUE)

library(tm)
library(ggplot2)
library(lsa)
library(scatterplot3d)
#library(SnowballC)
library(doMC)
library(wordcloud)

registerDoMC(cores = 4)

df <- data.frame(x$ID,x$Text)
#df$ID <- 1:nrow(df)
# prepare corpus'


#df<- df[-c(1),]
df <- df[!duplicated(df[,2]),]
str(df)




df <- df[c(1:3000),]

corpus <- Corpus(VectorSource(df[,2]))

corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))

# error below, can't stem
corpus  <- tm_map(corpus, stemDocument, language = "english") 
corpus  





#------------------------------------------------------------------------------

# MDS with raw term-document matrix compute distance matrix
td.mat <- as.matrix(TermDocumentMatrix(corpus))
td.mat
colnames(td.mat) <- df[,1]
dist.mat <- dist(t(as.matrix(td.mat)))
#dist.mat  # check distance matrix
#colnames(td.mat) <- 


#------------------------------------------------------------------------------

# MDS
fit <- cmdscale(dist.mat, eig = TRUE, k = 2)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y, color = df[,1])) #+ geom_text(data = points, aes(x = x, y = y - 0.2, label = row.names(df)))




#------------------------------------------------------------------------------

# MDS with LSA
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat)  # weighting
lsaSpace <- lsa(td.mat.lsa)  # create LSA space
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace)))  # compute distance matrix
#dist.mat.lsa  # check distance mantrix
#str(lsaSpace)
#str(dist.mat.lsa)

#------------------------------------------------------------------------------

# MDS
fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 2)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y =y, color = df[,1])) + geom_text(data = points, aes(x = x, y = y - 0.2, label = row.names(df)))

fit <- data.frame(fit$points)
fit$ID <- 1:nrow(fit)
str(fit)
str(df)

j <- merge(df,fit, by="ID")

#------------------------------------------------------------------------------

# plot
fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 3)
colors <- rep(c("blue", "green", "red"), each = 3, length(x))
scatterplot3d(fit$points[, 1], fit$points[, 2], fit$points[, 3], color = colors, 
              pch = 16, main = "Semantic Space Scaled to 3D", xlab = "x", ylab = "y", 
              zlab = "z", type = "h")

corpus <- Corpus(VectorSource(df[,2]))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, toSpace,"/")
corpus <- tm_map(corpus, toSpace,"@")
corpus <- tm_map(corpus, toSpace,"\\|")

corpus=tm_map(corpus,tolower)
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeNumbers)
corpus=tm_map(corpus,removeWords,stopwords("en"))
corpus=tm_map(corpus,removeWords, c("phone","u0001f3fb","omitted","image","audio","u0001f917"))
corpus=tm_map(corpus,stripWhitespace)
#corpus=Corpus(VectorSource(corpus))
tdm=TermDocumentMatrix(corpus)
m=as.matrix(tdm)
v=sort(rowSums(m),decreasing=T)
d=data.frame(words=names(v),freq=v)

wordcloud(d$words,d$freq,max.words=300,colors=brewer.pal(10,"Dark2"),rot.per = .35,random.order=F)
