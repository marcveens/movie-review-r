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

## Get all data from Mongo
con=mongo(collection="reviews",db="movies")
df <-con$find("{}")
df_count <- nrow(df)

## The seed makes the sample function predictable. The results will stay the same
set.seed(1)

## Clean the corpus
clean_corpus <- function(corpus) {
  corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)
}

## Randomize all results
df <- df[sample(nrow(df)), ]

corpus <- Corpus(VectorSource(df$text))

corpus.clean <- clean_corpus(corpus)

dtm <- DocumentTermMatrix(corpus.clean)

## Used for the statistics later on
df.train <- df[1:ceiling(df_count / 2),]
df.test <- df[(ceiling(df_count / 2) + 1):df_count,]

dtm.train <- dtm[1:ceiling(df_count / 2),]
dtm.test <- dtm[(ceiling(df_count / 2) + 1):df_count,]

corpus.clean.train <- corpus.clean[1:ceiling(df_count / 2)]
corpus.clean.test <- corpus.clean[(ceiling(df_count / 2) + 1):df_count]

fivefreq <- findFreqTerms(dtm.train, 5)

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))
dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)

classifier <- naiveBayes(trainNB, df.train$sentiment, laplace = 1)
pred <- predict(classifier, newdata=testNB)

table("Predictions"= pred,  "Actual" = df.test$sentiment )

conf.mat <- confusionMatrix(pred, df.test$sentiment)

conf.mat

custom_reviews <- c("very good", "very bad", "nice", "ingredients : possessed plastic dolls in love , plastic dolls having sex starring : jennifer tilly , voice of brad dourif , katherine heigl , nick stabile , john ritter synopsis : this is the fourth film in the chucky series , which debuted in the late 1980s . basically , chucky is a plastic doll that can walk and talk because it is possessed by the spirit of a slain murderer . in bride of chucky , chucky's longtime girlfriend tiffany ( jennifer tilly ) dies and her spirit inhabits a female plastic doll through voodoo . dolls tiffany and chucky get married and embark on a quest to reach a cemetery in new jersey , where a mystical gem might enable them to be humans . the dolls stow away in the back of a vehicle driven by a newly eloped couple , so in the side plot , the couple suspects each other of being a murderer . opinion : bride of chucky is an attempt at horror with humor , but doesn't succeed . somehow , chucky moaning about mid life crisis , and how he should have gotten married does not make for a very scary chucky . and tiffany harping about mid life crisis , and how she should have gotten married does not make for a very scary tiffany . the suspenseless bride of chucky relies mostly on jennifer tilly's cleavage to keep attention during the first half , and on occasional puns to keep attention during the second half . the best that can be said about bride of chucky is that it is sarcastic .", "hate", "love", "I really like this movie. Awesome")
custom_corpus <- Corpus(VectorSource(custom_reviews))
custom_corpus <- clean_corpus(custom_corpus)
custom_dtm <- DocumentTermMatrix(custom_corpus, control=list(dictionary = fivefreq))
custom_dtm <- apply(custom_dtm, 2, convert_count)
custom_pred <- predict(classifier, newdata=custom_dtm)
custom_pred
table("pred"=custom_pred)
