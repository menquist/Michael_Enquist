library(readxl)
library(plyr)
library(dplyr)
library(plyr)
library(ggplot2)
library(tidyr)
library(splitstackshape)
library(tm)
library(lsa)

#library(officer)

setwd("~/DOCS/Rachel")


#example_pptx <- system.file(package = "officer", "F2017 Q1 TO OGL Report v9.pptx")
#read_pptx()

#doc <- read_pptx("F2017 Q1 TO OGL Report v9.pptx")
#content <- pptx_summary(doc)
#content


AIMS <- read_excel("AIMS issues as of March 17 2017_all issues.xlsx",1)
ARCHER <- read_excel("Archer All issues_Mar162017.xlsx",1)
CORE <- read_excel("Core Issues as of March 6 2017 (2).xlsx",1)
ERSC <- read_excel("ERSC ActionPlan_Extract_Feb 22 2017.xlsx",1)
#FLATFILE.x <- read_excel("Copy of TO_Findings_Flatfile _Working Copy.xlsx",1)
FLATFILE <- read.csv("Copy of TO_Findings_Flatfile _Working Copy.csv")

OLG <- read_excel("OLG.xlsx")

##################### FLATFILE
names(FLATFILE)
str(FLATFILE)
FLATFILE <- FLATFILE[,c(1:71)]
FLATFILE$ID <- 1:nrow(FLATFILE)
FLATFILE$QA <- "FLATFILE"
FLATFILE$QA.ID <- with(FLATFILE, paste(ID,QA,  sep = "."))
########## move columns to the front
refcols <- c("QA.ID")
FLATFILE <- FLATFILE[, c(refcols, setdiff(names(FLATFILE), refcols))]
names(FLATFILE)
#take out columns
cols <- c("ID","QA")
FLATFILE <- FLATFILE %>%
  select(-one_of(cols))

FLATFILE <- FLATFILE[!is.na(FLATFILE$FQtr),]


FLATFILE.x <- FLATFILE
#write.csv(FLATFILE,"TO_Findings_Flatfile _Working Copy(QA.ID).csv")  




FLATFILE$LOD.Name <- as.factor(FLATFILE$LOD.Name)
l <- levels(FLATFILE$LOD.Name )
l

FLATFILE <- FLATFILE[with(FLATFILE, grepl("1st|2nd",LOD.Name )),]

FLATFILE.1 <-FLATFILE

FLATFILE <- FLATFILE %>%
  select("QA.ID","Issue","Issue.Description" )


colnames(FLATFILE)[2] <- "Issue Title"
colnames(FLATFILE)[3] <- "Issue Description"

FLATFILE$`Issue Title` <- as.character(FLATFILE$`Issue Title`)
FLATFILE$`Issue Description` <- as.character(FLATFILE$`Issue Description`)

#y <- FLATFILE[!duplicated(FLATFILE$`Issue Description`),]

################################# AIMS
names(AIMS)
str(AIMS)
AIMS$ID <- 1:nrow(AIMS)
AIMS$QA <- "AIMS"
AIMS$QA.ID <- with(AIMS, paste(ID,QA,  sep = "."))
########## move columns to the front
refcols <- c("QA.ID")
AIMS <- AIMS[, c(refcols, setdiff(names(AIMS), refcols))]
names(AIMS)
#take out columns
cols <- c("ID","QA")
AIMS <- AIMS %>%
  select(-one_of(cols))

#write.csv(AIMS,"AIMS issues as of March 17 2017_all issues(QA.ID).csv")  



AIMS <- AIMS %>%
  select("QA.ID","Issue Title","Issue Description","Risk Level/Rating")
AIMS$`Risk Level/Rating` <- as.factor(AIMS$`Risk Level/Rating`)
l <- levels(AIMS$`Risk Level/Rating`)
l

#Example <- PRC.Library[with(PRC.Library, grepl("SOX",Control.L2..Activity.Name. )|grepl("SOX", Control.L2..Activity.Description.)),]
AIMS <- AIMS[with(AIMS, grepl("Significant",`Risk Level/Rating` )),]

AIMS <- AIMS[,-c(4)]

####################### ERSC
names(ERSC)
str(ERSC)
ERSC$ID <- 1:nrow(ERSC)
ERSC$QA <- "ERSC"
ERSC$QA.ID <- with(ERSC, paste(ID,QA,  sep = "."))

########## move columns to the front
refcols <- c("QA.ID")
ERSC <- ERSC[, c(refcols, setdiff(names(ERSC), refcols))]
names(ERSC)
#take out columns
cols <- c("ID","QA")
ERSC <- ERSC %>%
  select(-one_of(cols))

#write.csv(ERSC,"ERSC ActionPlan_Extract_Feb 22 2017(QA.ID).csv")  


#take out duplicates
ERSC <- ERSC[!duplicated(ERSC$`Issue ERCS Name`),]

ERSC <- ERSC %>%
  select("QA.ID","Issue Title","Issue Description","Issue Severity Rating")
ERSC$`Issue Severity Rating` <- as.factor(ERSC$`Issue Severity Rating`)

l <- levels(ERSC$`Issue Severity Rating`)
l

ERSC <- ERSC[with(ERSC, grepl("Significant",`Issue Severity Rating` )),]

ERSC <- ERSC[,-c(4)]


################################################# CORE

names(CORE) 
CORE$ID <- 1:nrow(CORE)
CORE$QA <- "CORE"
CORE$QA.ID <- with(CORE, paste(ID,QA,  sep = "."))

########## move columns to the front
refcols <- c("QA.ID")
CORE <- CORE[, c(refcols, setdiff(names(CORE), refcols))]
names(CORE)
#take out columns
cols <- c("ID","QA")
CORE <- CORE %>%
  select(-one_of(cols))
# Archer All issues_Mar162017
#write.csv(CORE,"Core Issues as of March 6 2017 (2)(QA.ID).csv")  


str(CORE)

CORE <- CORE %>%
  select(QA.ID, "Issue Title", "Issue Description")

CORE <- CORE[!duplicated(CORE$`Issue Description`),]

############################################### Archer

names(ARCHER) 
ARCHER$ID <- 1:nrow(ARCHER)
ARCHER$QA <- "ARCHER"
ARCHER$QA.ID <- with(ARCHER, paste(ID,QA,  sep = "."))

########## move columns to the front
refcols <- c("QA.ID")
ARCHER <- ARCHER[, c(refcols, setdiff(names(ARCHER), refcols))]
names(ARCHER)
#take out columns
cols <- c("ID","QA")
ARCHER <- ARCHER %>%
  select(-one_of(cols))
# Archer All issues_Mar162017
#write.csv(ARCHER,"Archer All issues_Mar162017(QA.ID).csv")  




ARCHER <- ARCHER %>%
  select(QA.ID,"Finding Short Name","Finding","Overall Status","Op Risk Issue Rating" )


ARCHER$`Overall Status` <- as.factor(ARCHER$`Overall Status`)
l <- levels(ARCHER$`Overall Status`)
l
ARCHER <- ARCHER[with(ARCHER, grepl("^Remediation - Pending Closure$",`Overall Status` )|grepl("^Risk Acceptance - Risk Accepted$", `Overall Status`)),]



ARCHER$`Op Risk Issue Rating` <- as.factor(ARCHER$`Op Risk Issue Rating`)
l <- levels(ARCHER$`Op Risk Issue Rating`)
l
ARCHER <- ARCHER[with(ARCHER, grepl("Significant",`Op Risk Issue Rating`)),]

ARCHER <- ARCHER[,-c(4,5)]
colnames(ARCHER)[2] <- "Issue Title" 
colnames(ARCHER)[3] <-"Issue Description" 

##################### colsolidated

EXTRACT <- rbindlist(list(AIMS,CORE,ERSC,ARCHER,OLG ), fill=T)

#EXTRACT <- EXTRACT[!duplicated(EXTRACT$`Issue Description`)]


#different.names <- (!FLATFILE$`Issue Title` %in% EXTRACT$`Issue Title`)
#not.in.EXTRACT <- FLATFILE[!different.names,]
#not.in.EXTRACT <- not.in.EXTRACT[!duplicated(not.in.EXTRACT$`Issue Description`),]

#not.in.EXTRACT$QA.Results <- "Flagged"
#FLATFILE.1$QA.Results <- ""

#colnames(FLATFILE.1)[25] <- "Issue Title"
#colnames(FLATFILE.1)[26] <- "Issue Description"

#merge.Flatfile <- merge(FLATFILE.1,not.in.EXTRACT, by="QA.ID")



#take out columns
#cols <- c("Issue Title.y","Issue Description.y" ,"LOD.Name.y","QA.Results.y")
#merge.Flatfile <- merge.Flatfile %>%
#  select(-one_of(cols))
#colnames(merge.Flatfile)[142] <- "QA.Results"

### Replace rows in one data frame if they appear in another data frame

FLATFILE.1[match(merge.Flatfile$QA.ID, FLATFILE.1$QA.ID), ] <- merge.Flatfile

#diff <- df.diff(main[EXTRACT$`Issue Description`], lookup[FLATFILE$`Issue Description`])
# colsolidate FLATFILE and Flatfile 
#diff <- FLATFILE[!FLATFILE$`Issue Description` %in% EXTRACT$`Issue Description` ]

EXTRACT <- rbindlist(list(EXTRACT,FLATFILE ), fill=T)



# remove blanks and NA values
EXTRACT <- EXTRACT[!(is.na(EXTRACT$`Issue Description`) | EXTRACT$`Issue Description`==""), ]

#EXTRACT <- EXTRACT[!is.na(EXTRACT$`Issue Description`),]


######################## Semantic Analysis
# if you need to take more core out of your computer. This process will run the script faster by 
# taking more memory. FYI make sure you turn off the script
library(doParallel)

#system.time(foreach(i=1:10000) %do% sum(tanh(1:i)))
#system.time(foreach(i=1:10000) %dopar% sum(tanh(1:i)))
#registerDoParallel()  
#getDoParWorkers()
#registerDoSEQ()  
#getDoParWorkers()
#
#Registering the CPU cores
registerDoParallel(cores=4)  
getDoParWorkers()



# create memory  ( reasearch more later)
cl <- makeCluster(100)  

registerDoParallel(cl) 


stopCluster(cl)

# prepare corpus
corpus <- Corpus(VectorSource(EXTRACT$`Issue Description`))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
corpus <- tm_map(corpus, stemDocument, language = "english")
corpus  # check corpus

#corpus <- docs
## A corpus with 9 text documents
# 2. MDS with raw term-document matrix compute distance matrix
# Compute a term-document matrix that contains occurrance of terms
# Compute distance between pairs of documents and scale the multidimentional semantic space (MDS) onto two dimensions

td.mat <- as.matrix(TermDocumentMatrix(corpus))
dist.mat <- dist(t(as.matrix(td.mat)))
#dist.mat  # check distance matrix

#dist.mat  # check distance matrix
#stopCluster(cl)  
# Compute distance between pairs of documents and scale the multidimentional semantic space onto two dimensions
fit <- cmdscale(dist.mat, eig = TRUE, k = 2)
str(fit)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
#str(points)
#color = df$view
#ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y)) + geom_text(data = points, aes(x = x, y = y - 0.2, label = row.names(EXTRACT)))
# 3. MDS with LSA
#td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat)  # weighting
#s <- td.mat.lsa
#share <- .5
#any(which(cumsum(s/sum(s)) <= share)) 
#cumsum(s/sum(s)) <= share
#d = max(which(cumsum(s/sum(s)) <= share)) + 1
#d

#s = svd(td.mat.lsa)$d

# standard share of 0.5
#dimcalc_share()(s) 

# specific share of 0.9
#dimcalc_share(share=0.9)(s) 

# meeting the number of documents 
#n = ncol(td.mat.lsa)
#dimcalc_ndocs(n)(s)


#lsaSpace <- lsa(td.mat.lsa, dims = dimcalc_ndocs(n)(s))
#td.mat.lsa <- lw_logtf(td.mat) * gw_idf(td.mat)  # weighting log
#lsaSpace <- lsa(td.mat.lsa)
#data(stopwords_en)
#myMatrix = textmatrix(td, stopwords=stopwords_en)
#dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace)))  # compute distance matrix
#dist.mat.lsa  # check distance mantrix

#summary.textmatrix(dist.mat.lsa)

#cosine(dist.mat.lsa)


#library(scattterplot3d)
#setRepositories(scattterplot3d)
#ap <- available.packages()
#View(ap)


# MDS  color = df$view
#fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 2)
#points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
#ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y)) + geom_text(data = points, aes(x = x, y = y - 0.2, label = row.names(EXTRACT)))
#library(qgraph)
#q1 = qgraph(dist.mat.lsa, borders= TRUE, cut=80, minimum =50, label.cex= 4, layout="spring", label.norm = "0000")

# vsize = 3, cut =5


#write.csv(dup,"GPO.duplicate.csv")

# Using the SnowballC library to stem text
docs <- corpus 
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
#str(d)

# Using the SnowballC library to stem text
#df$ID <- 1:nrow(df)


#df$combine.ID <- with(df, paste( combine.ID,ID,  sep = "."))



colnames(m) <- EXTRACT$QA.ID
str(m)
#d$freq <- as.integer(d$freq)
# Generate the WordCloud
#library("wordcloud")
#library(RColorBrewer)
#par(bg="grey30") 
#png(file="WordCloud.png",width=1000,height=700, bg="grey30")
#wordcloud(d$word, d$freq, col=terrain.colors(length(d$word), alpha=0.9), random.order=FALSE, rot.per=0.3 )
#title(main = "Hillary Clinton's Most Used Used in the Emails", font.main = 1, col.main = "cornsilk3", cex.main = 1.5)
#dev.off()
#write.csv(points,"PRC.points.csv")

#points <- read.csv("PRC.points.csv")

#write.csv(m,"PRC.Cosine.graph.csv")

#####################################################################################


library(igraph)

#write.csv(d, "Procurement.Controls.Most.Common.Keywords.csv")


cosineSim_mat <- cosine(m)
diag(cosineSim_mat) <- 0
cosineSim_mat[(cosineSim_mat < 0.5)] <-0 # prune the graph. Similarities of strength <0.5 are set to 0
cosineSim_mat[(cosineSim_mat >= 0.5)] <- 1
cosineSim_graph <- graph.adjacency(cosineSim_mat, mode = 'undirected', weighted = TRUE) # convert the matrix to an igraph object
#plot(cosineSim_graph) 
#cosineSim_graph
str(cosineSim_graph)


#n <- sample_pa(1469)
#x <- cluster_edge_betweenness(n)
#str(x) # list
#collect <- x[ 1:length(x) ]

#str(collect)
#collect.df = ldply(collect,function(t) t$toDataframe())
#collect.df <- do.call("rbind",lapply(collect,as.data.frame))

#str(collect.df)
#write.csv(dup,"GPO.duplicate.csv")
#write.csv(collect.df,"EXTRACT.Duplicated.Consolidated.csv")

#collect.df = ldply(collect,function(t) t$toDataframe())
#collect.df <- do.call("rbind",lapply(cosineSim_graph,as.data.frame))

compg.edges <- as.data.frame(get.edgelist(cosineSim_graph))
compg.edges.1 <- as.data.frame(get.edgelist(cosineSim_graph))
#str(compg.edges.1)

#write.csv(compg.edges,"EXTRACT.Similar.Consolidated.edges.csv")

########## Looking into every cell
#melt.access$ID <- 1:nrow(melt.access) 
#access$ID <- 1:nrow(access) 

points$ID.con <- 1:nrow(points)
EXTRACT$ID.con <- 1:nrow(EXTRACT)


data.points <- merge(EXTRACT, points, by = "ID.con")
#part.2 <- merge(melt.access, data.points, by = "combine.ID")

#write.csv(data.points,"EXTRACT.Similar.Consolidted.csv")

########## move columns to the front
#col_idx <- grep("^text$|^combine.ID$|^ID$",  names(part.2))
#part.2 <- part.2[, c(col_idx, (1:ncol(part.2))[-col_idx])]
#names(part.2)



#find duplicates
#n_occur <- data.frame(table(data.points$`Issue Description`))
#n_occur[n_occur$Freq > 1,]
#dup <- data.points[data.points$`Issue Description` %in% n_occur$Var1[n_occur$Freq > 1],]
#dup


#write.csv(dup,"EXTRACT.Similar.Consolidted.Duplicates.csv")

#duplicated(part.2$x)

#Look.at.duplicates <- part.2[duplicated(part.2$text),]
#str(Look.at.duplicates)




###############################################
str(compg.edges)
str(mod)
colnames(compg.edges)[1] <- "QA.ID"
colnames(compg.edges.1)[2] <- "QA.ID"
compg.edges$QA.ID <- as.character(compg.edges$QA.ID)
compg.edges.1$QA.ID <- as.character(compg.edges.1$QA.ID)

#catch <- merge(compg.edges, mod, by="combine.ID",  all=T, sort=FALSE)
#catch.1 <- merge(compg.edges.1, mod, by="combine.ID",  all=T, sort=FALSE)

selection <- inner_join(compg.edges,data.points, by="QA.ID")

selection.1 <- inner_join(compg.edges.1,EXTRACT, by="QA.ID")
str(selection)
str(selection.1)

#selection <- catch
#selection.1 <- catch.1
#create unique ID
selection$ID <-  1:nrow(selection)
selection.1$ID <-  1:nrow(selection.1)

selection.3 <- inner_join(selection,selection.1, by="ID")
names(selection.3)



########## move columns to the front
refcols <- c("QA.ID.x","QA.ID.y","Issue Title.x","Issue Title.y","Issue Description.x","Issue Description.y","x","y")
selection.3 <- selection.3[, c(refcols, setdiff(names(selection.3), refcols))]
names(selection.3)

x <- selection.3[!is.na(selection.3$`Issue Description.x`),]
x <- x[!is.na(x$`Issue Description.y`),]

x <- x[,-c(9:13)]

library(stringdist)

x$distance <- stringdist(x$`Issue Description.x`,x$`Issue Description.y`,method="jaccard")

# word count between each row

#word.count <- x[, Count := grepl(x$`Issue Description.x`, x$`Issue Description.y`), by = x$`Issue Description.x`]

#library(stringr)

# Count the number of 'a's in each element of string
#x$word.count <- str_count(x$`Issue Description.x`, as.character(semantics$word))

#x$word.count.1 <-  as.integer(str_detect(x$`Issue Description.x`, paste0("\\b", x$`Issue Description.y`, "\\b")))


#x$word.match <- as.integer(!is.na(mapply(match, x$`Issue Description.x`, strsplit(x$`Issue Description.y`, ' '))))



#allwords <- lapply(w, function(z) data.frame(lineNum = 1:length(x$`Issue Description.x`), 
#                                             count = sapply(x$`Issue Description.x`, function(x) sum(str_count(x, z)))))
######## To text mine whole words instead of stemwords

library(stringi)
# Load the data as a corpus
docs <- Corpus(VectorSource(EXTRACT$`Issue Description`))
#inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

semantics <- d %>% 
  filter(freq >=2)

w <- as.character(semantics$word)

# count words in each row
x$count <- stri_count_regex(x$`Issue Description.x`, paste(w, collapse="|"))



refcols <- c("QA.ID.x","QA.ID.y","Issue Title.x","Issue Title.y","Issue Description.x","Issue Description.y","distance","count","x","y")
x <- x[, c(refcols, setdiff(names(x), refcols))]
names(x)

#names(allwords) <- w

# write csv
write.csv(x,"EXTRACT-QA.csv")

########################################## QA Flatfile
x$QA.ID.x <- as.factor(x$QA.ID.x)
x$QA.ID.y <- as.factor(x$QA.ID.y)
l <- levels(x$QA.ID.x)
l


dat <- x[with(x, grepl("FLATFILE", QA.ID.x )),]
#dat.1 <- x[with(x, grepl("FLATFILE", QA.ID.y )),]
dat.a <- x[with(x,grepl("FLATFILE",QA.ID.x ) & !grepl("FLATFILE", QA.ID.y)),]
dat.b <- x[with(x,grepl("FLATFILE",QA.ID.y ) & !grepl("FLATFILE", QA.ID.x)),]





Flat <- data.frame(dat.a$QA.ID.x,0)
Flat.1 <- data.frame(dat.b$QA.ID.y,0)

colnames(Flat)[1] <- "QA.ID"
colnames(Flat.1)[1] <- "QA.ID"

file <- rbindlist(list(Flat,Flat.1 ), fill=T)

file <- file[!duplicated(file$QA.ID),]

merge <- merge(FLATFILE.1,file, by="QA.ID" )
merge <- merge[,-c(73)]

merge$QA.Results <- "Recognized"
FLATFILE.1$QA.Results <- "Not Recognized"
FLATFILE.x$QA.Results <- ""
#different.names <- (!FLATFILE$`Issue Title` %in% EXTRACT$`Issue Title`)
#not.in.EXTRACT <- FLATFILE[!different.names,]
#not.in.EXTRACT <- not.in.EXTRACT[!duplicated(not.in.EXTRACT$`Issue Description`),]

a <- FLATFILE.x
FLATFILE.1[match(merge$QA.ID, FLATFILE.1$QA.ID), ] <- merge
FLATFILE.x[match(FLATFILE.1$QA.ID, FLATFILE.x$QA.ID), ] <- FLATFILE.1


########## move columns to the front
refcols <- c("QA.Results")
FLATFILE.x <- FLATFILE.x[, c(refcols, setdiff(names(FLATFILE.x), refcols))]
names(FLATFILE.x)

write.csv(FLATFILE.x,"TO_Findings_Flatfile _Working Copy(QA.ID).csv")  

#
