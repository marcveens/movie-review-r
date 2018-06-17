## Dataset 1: https://www.kaggle.com/c/word2vec-nlp-tutorial
## Dataset 2: https://www.kaggle.com/nltkdata/movie-review

# install.packages("readtext")
# install.packages("dplyr")
# install.packages("mongolite")

library(readtext)
library(dplyr)
library(mongolite)

getValuesFromTxt <- function(files, sentiment) {
  ds <- readtext::readtext(files)
  ds$doc_id <- NULL;
  ds$sentiment <- sentiment;
  colnames(ds)[1] <- "text"
  ds
}

setwd("C:\Users\MarcVe\OneDrive\Documenten\HvA\2017\Data analysis\Individual")

data <- read.csv2("dataset/1/labeledTrainData.csv", quote="", sep="\t")
data$id <- NULL
colnames(data)[2] <- "text"

dataset2neg <- getValuesFromTxt("dataset/2/neg/*.txt", 0)
dataset2pos <- getValuesFromTxt("dataset/2/pos/*.txt", 1)

data <- bind_rows(data, dataset2neg)
data <- bind_rows(data, dataset2pos)

## Randomize all data rows so there aren't humps of successive positive and negatives
data <- data[sample(nrow(data), nrow(data)), ]

data$doc_id <- seq.int(nrow(data))

## Save to Mongo
con=mongo(collection="reviews",db="movies")
con$drop()
con$insert(data)

