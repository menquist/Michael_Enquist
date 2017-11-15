library(RCurl)
test_data_url <- "https://dl.dropboxusercontent.com/u/8082731/datasets/UMICH-SI650/testdata.txt"
train_data_url <- "https://dl.dropboxusercontent.com/u/8082731/datasets/UMICH-SI650/training.txt"

test_data_file <- getURL(test_data_url)
train_data_file <- getURL(train_data_url)

train_data_df <- read.csv(
  text = train_data_file, 
  sep='\t', 
  header=FALSE, 
  quote = "",
  stringsAsFactor=F,
  col.names=c("Sentiment", "Text"))
test_data_df <- read.csv(
  text = test_data_file, 
  sep='\t', 
  header=FALSE, 
  quote = "",
  stringsAsFactor=F,
  col.names=c("Text"))
# we need to convert Sentiment to factor
train_data_df$Sentiment <- as.factor(train_data_df$Sentiment)

str(train_data_df)

mean(sapply(sapply(train_data_df$Text, strsplit, " "), length))

#porpus


library(tm)
corpus <- Corpus(VectorSource(c(train_data_df$Text, test_data_df$Text)))
corpus[1]$content

corpus <- tm_map(corpus, content_transformer(tolower))
# the following line may or may not be needed, depending on
# your tm  package version
# corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)

corpus[1]$content

str(corpus)

dtm <- DocumentTermMatrix(corpus)
dtm

sparse <- removeSparseTerms(dtm, 0.99)
sparse

important_words_df <- as.data.frame(as.matrix(sparse))
colnames(important_words_df) <- make.names(colnames(important_words_df))
# split into train and test
important_words_train_df <- head(important_words_df, nrow(train_data_df))
important_words_test_df <- tail(important_words_df, nrow(test_data_df))

# Add to original dataframes
train_data_words_df <- cbind(train_data_df, important_words_train_df)
test_data_words_df <- cbind(test_data_df, important_words_test_df)

# Get rid of the original Text field
train_data_words_df$Text <- NULL
test_data_words_df$Text <- NULL

#Bag of words

library(caTools)
set.seed(1234)
# first we create an index with 80% True values based on Sentiment
spl <- sample.split(train_data_words_df$Sentiment, .85)
# now we use it to split our data into train and test
eval_train_data_df <- train_data_words_df[spl==T,]
eval_test_data_df <- train_data_words_df[spl==F,]

log_model <- glm(Sentiment~., data=eval_train_data_df, family=binomial)
summary(log_model)
log_pred <- predict(log_model, newdata=eval_test_data_df, type="response")
# Calculate accuracy based on prob
table(eval_test_data_df$Sentiment, log_pred>.5)
(453 + 590) / nrow(eval_test_data_df)
log_pred_test <- predict(log_model, newdata=test_data_words_df, type="response")
test_data_df$Sentiment <- log_pred_test>.5

set.seed(1234)
spl_test <- sample.split(test_data_df$Sentiment, .0005)
test_data_sample_df <- test_data_df[spl_test==T,]
test_data_sample_df[test_data_sample_df$Sentiment==T, c('Text')]
test_data_sample_df[test_data_sample_df$Sentiment==F, c('Text')]
