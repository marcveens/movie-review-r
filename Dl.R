# https://rpubs.com/cen0te/naivebayes-sentimentpolarity

# install.packages("mongolite")
# install.packages("tm")
# install.packages("RTextTools")
# install.packages("e1071")
# install.packages("dplyr")
# install.packages("caret")

# Load required libraries

library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)
library(mongolite)

con=mongo(collection="reviews",db="movies")

df <-con$find("{}")
df_count <- nrow(df)

set.seed(1)
df <- df[sample(nrow(df)), ]
df <- df[sample(nrow(df)), ]

df$sentiment <- as.factor(df$sentiment)

corpus <- Corpus(VectorSource(df$text))

corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)

dtm <- DocumentTermMatrix(corpus.clean)

df.train <- df[1:ceiling(df_count / 2),]
df.test <- df[(ceiling(df_count / 2) + 1):df_count,]

dtm.train <- dtm[1:ceiling(df_count / 2),]
dtm.test <- dtm[(ceiling(df_count / 2) + 1):df_count,]

corpus.clean.train <- corpus.clean[1:ceiling(df_count / 2)]
corpus.clean.test <- corpus.clean[(ceiling(df_count / 2) + 1):df_count]

dim(dtm.train)

## Custom test set
doc_id <- c(10000,10001,10002,10003,10004,10005,10006)
text <- c("very good", "very bad", "nice", "like", "hate", "love", "I really like this movie. Awesome")
sentiment <- c(1, 0, 1, 1, 0, 1, 1)
sentiment <- as.integer(sentiment)
df.test <- data.frame(doc_id, text, sentiment)
test_corpus <- Corpus(VectorSource(df.test$text))
test_corpus.clean <- test_corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)
dtm.test <- DocumentTermMatrix(test_corpus.clean)
corpus.clean.test <- test_corpus.clean[]





fivefreq <- findFreqTerms(dtm.train, 5)
length((fivefreq))
## [1] 12144

# Use only 5 most frequent words (fivefreq) to build the DTM

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))

dim(dtm.train.nb)
## [1]  1500 12144

dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))

dim(dtm.train.nb)

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)

system.time( classifier <- naiveBayes(trainNB, df.train$sentiment, laplace = 1) )
system.time( pred <- predict(classifier, newdata=testNB) )

table("Predictions"= pred,  "Actual" = df.test$sentiment )

conf.mat <- confusionMatrix(pred, df.test$sentiment)

conf.mat