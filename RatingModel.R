## Need to use classification in order to categorize new reviews
## Supervised learning, because training based on human classified dataset
## Semantic parsing in order to get context from text
## Sentiment

## Accuracy should be 75%, calculated by (TP+TN)/(TP+FP+FN+TN)

# install.packages("dplyr")
# install.packages("mongolite")
# install.packages("tm")

library(dplyr)
library(mongolite)
library(tm)

con=mongo(collection="reviews",db="movies")

data <-con$find("{}")


## Corpus of all positive reviews
data_positive <-con$find('{"sentiment": 1}')
dp_source <- DataframeSource(data_positive)
dp_corpus <- VCorpus(dp_source)

## Filter out all stopwords
dp_corpus <- tm_map(dp_corpus, removeWords, c("movie", stopwords("en")))
dp_corpus <- tm_map(dp_corpus, removePunctuation)

## Apply stemCompletion
dp_corpus <- 

## Strip all whitespaces
dp_corpus <- tm_map(dp_corpus, stripWhitespace)

dp_corpus[[1]][1]

## Corpus of all negative reviews