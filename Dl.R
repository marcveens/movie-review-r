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
  tm_map(removeWords, stopwords(kind="en"))
}

## Prepare the set for NaiveBayes
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

## Randomize all results
df <- df[sample(nrow(df)), ]

## Convert sentiment to factor instead of decimal
df$sentiment <- as.factor(df$sentiment)

## Interpret each element of vector df$text as a document & create the corpus
corpus <- Corpus(VectorSource(df$text))

## Clean the corpus,
corpus_clean <- clean_corpus(corpus)

## Create a documenttermmatrix of the cleaned corpus
dtm <- DocumentTermMatrix(corpus_clean)

## Used for the statistics later on
df.train <- df[1:ceiling(df_count / 4 * 3),]
df.test <- df[(ceiling(df_count / 4 * 3) + 1):df_count,]

dtm.train <- dtm[1:ceiling(df_count / 4 * 3),]
dtm.test <- dtm[(ceiling(df_count / 4 * 3) + 1):df_count,]

corpus.clean.train <- corpus_clean[1:ceiling(df_count / 4 * 3)]
corpus.clean.test <- corpus_clean[(ceiling(df_count / 4 * 3) + 1):df_count]

fivefreq <- findFreqTerms(dtm.train, 420)

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))
dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))

trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)

classifier <- naiveBayes(trainNB, df.train$sentiment, laplace = 1)
pred <- predict(classifier, newdata=testNB)

table("Predictions"= pred,  "Actual" = df.test$sentiment )

conf.mat <- confusionMatrix(pred, df.test$sentiment)

conf.mat

custom_reviews <- c("As you expected, Deadpool 2 is all that we expected and above. The action does not stop almost the entire movie, the new characters are great especially Cable and Domino. The humor as you expected is something without which the movie will not be the same ... yet it is a distinctive feature for the character of Deadpool. The story is good and you can expect some surprising people to interfere in the the movie, but I will not tell you, see for yourself. The effects are very good, the music choice fits great with the action scenes and the individual moments in the movie.", ## pos
                    "Your going to be hard pressed to find a movie more over the top than Deadpool 2, David leitch takes it to a whole new level entirely. It works namely because the violence here is just to creative. Sure it defies all Logic most of the time, but i was laughing and having a rip-roaring time throughout. Go into this movie knowing that it is a campy, corny, over the top superhero/action/comedy that is just about crazy shooting sequences and one-liners, and you'll be fine. Do not go into this movie expecting deep plot, meaningful conversations among characters, or anything remotely resembling a serious action or drama movie.", ## pos
                    "Summer movies often hype themselves as spectacular events not to be missed and their ad campaigns use words like \"epic\", \"spectacle\", and \"smash\" as ways to build the hype to increase advanced box office sales. The summer 2018 film season kicks off in a big way with \"Avengers: Infinity War\" and it is the rare summer film that exceeds is lofty expectations and delivers a crowning achievement for the Marvel Cinematic Universe. When Thanos (Josh Brolin), embarks on a deadly campaign to find and possess the Infinity Stones, he leaves a path of death and destruction in his path. When his pursuit leading him to Earth, Bruce Banner (Mark Ruffalo), and Doctor Strange (Benedict Cumberbatch), who enlist a reluctant Tony Stark (Robert Downey Jr.), to the cause just as the minions of Thanos arrive. With The Avengers fractured and scattered following the events of \"Captain America: Civil War\" the teams find themselves dealing with the threat in various groups as fate steps in to divide many key members of the team. This allows for a great entry by the Guardians of the Galaxy and allows the film to take a very enjoyable path. Essentially the movie weaves separate storylines into one cohesive and very satisfying epic. You have a story with the Guardians as well as one with many of the key Avengers, as well as others with characters old and new. As such it is like a Guardians, Avengers, and Marvel film all rolled into one and each one supports the other very well yet has the charm and identity you would expect. While the tone is very dark as you would expect with literally half of the known universe facing destruction, there is also some solid humor in the film that never once undermines the story or pacing. Naturally the effects are stunning as you would expect as Marvel has put a significant amount of money into the film and it shows in every eye-popping action sequence. What really impressed me the most was that the Russo Brothers never let the FX of the film overshadow the characters and with a very large cast, it was nice to see everyone got their moment and you did not see members of the cast trying to one up each other. There are some real surprise moments along the way and the action is easily the best of any Marvel film to date. Many fans had expressed disappointment with \"Avengers: Age of Ultron\" for me this film is significantly better than the prior film and is everything that a Marvel fan would want in a film. I was also surprised by how well Thanos was portrayed as while you hopefully will not agree with his deductions and methods; you could actually understand his motivations and it help him transcend from the usual megalomaniacs which are so common in films of his type. I am really looking forward to seeing what comes next and make sure you stay through the credits for a bonus scene. Believe the hype, \"Avengers: Infinity War\" is an epic not to be missed and is an example of what a summer event film should be. 5 stars out of 5", ## pos
                    "Hello, For one thing I have to say that I have waited since last year to see this movie. I have been reading a couple of negative reviews, but I said well that was going to happen lol. A lot of people were already predispose with oceans 8 because it had been done by Clooney and Pitt which I love. The problem is this movie is great, in my opinion it's not imitating oceans 11, it's a movie on its own with its own heist. I love the movie I had certain reservations when it comes to Rihanna in the movie going up against Bullock or blanchett which I love or Hathaway which in my opinion those 3 stars are very good and let's not forget Helena Bonham Carter she was brilliant in this. Okay I am deviating Rihanna was okay meshed well with the others. I love Sandra I thought she had the same arrogant good kind of cockiness I am in control thing going, was hilarious with her schemes loved it. Cate was great there was a point when I thought before seeing the movie that. Cate should have been the leader, but I saw the movie and I am happy that Bullock pulled it off. My two favorite actresses Sandra Bullock and Cate Blanchett. Anyhow the movie was great all 8 actresses worked well, I rate it a 10 because I love the ingenuity, the plot it was fun. I saw this movie for what it was, a fun movie that it is not copying oceans 11 or anything, I saw it for what it is a entertaining movie that I enjoyed. I hope that we see more sequels in regards to this movie. I do think that if you give it a shot you might like it forget all those user that were already predispose to this movie, they are not giving it a fair shot.",## pos
                    "I have never seen such an amazing film since I saw The Shawshank Redemption. Shawshank encompasses friendships, hardships, hopes, and dreams. And what is so great about the movie is that it moves you, it gives you hope. Even though the circumstances between the characters and the viewers are quite different, you don't feel that far removed from what the characters are going through. It is a simple film, yet it has an everlasting message. Frank Darabont didn't need to put any kind of outlandish special effects to get us to love this film, the narration and the acting does that for him. Why this movie didn't win all seven Oscars is beyond me, but don't let that sway you to not see this film, let its ranking on the IMDb's top 250 list sway you, let your friends recommendation about the movie sway you. Set aside a little over two hours tonight and rent this movie. You will finally understand what everyone is talking about and you will understand why this is my all time favorite movie.", ## pos
                    "I didn't think an all women movie could be any worse than the awful new Ghostbusters. But, I was wrong. Sandra Bullock once again reminds us all why she is the most boring actress still alive. Mono-tone acting and painful script writing by all. Avoid.", ## neg
                    "Good heavens... I didn't make it far into this embarrassing bilge-water. This desperate attempt to be like the men falls flat on its' clean shaven face. I suppose a few frustrated women will get a kick out of it, but most movie goers are going to feel ripped off by this idiotic and preposterous mess. The big names are reeling in the cash and sacrificing their legacies and dignity to perform in this circus act. Non of them look like they want to be there. I've seen more on-screen chemistry in an episode of The Teletubbies! I'd be more than a little annoyed if I had paid to watch this!", ## neg
                    "The filmmakers had absolutely no idea what they wanted out of this movie. You can tell it took 44 producers and 8 years to make. Who was the protagonist? What style did they want to adopt? Is it a documentary? A character piece? A chronology of John Gotti's life? I couldn't tell. There was 4 time lines happening at once. Way to many names and places. It was simply boring and had no suspense. However, Travolta's performance wasn't bad, the script was. He definitly did the best he could with what he was given. Long story short, I have no more affection or knowledge of the Gotti family now than I did before the movie. Go watch the Sopranos instead.", ## neg
                    "Yes, my wife and I laughed once in the first 25 minutes of this dreadful, brainless, and frighteningly unfunny film (and that was when Melissa McCarthy's character gets blown across the lawn). I realize that this film appeals to the funny bones of some viewers, but objectively speaking it is a very sorry excuse for a comedy. We both like Melissa McCarthy. We really enjoyed her in \"The Heat,\" \"Bridesmaids,\" and \"St Vincent\" where she really got to act. She's been great on \"Saturday Night Live.\" But in this film, she's got lousy material to work with and she sinks to its level. With one laugh in 25 minutes, we availed ourselves of the local theater's 30-minute satisfaction guarantee and walked out of the theater and received a refund of the ticket price. It's the first time we've done that since \"Dennis the Menace\" in 1993.", ## neg
                    "This was the worst movie I have ever seen. And I'm not saying that as an exaggeration. This was the absolute worst movie I have ever seen. In my life. Do. Not. Waste. Any. Time. Seeing. This. You are better off taking the 8-10 bucks and ripping it up and throwing it away so you can't go see this movie") ## neg
custom_reviews <- data.frame(custom_reviews)
colnames(custom_reviews)[1] <- "text"
custom_corpus <- Corpus(VectorSource(custom_reviews$text))
custom_corpus <- clean_corpus(custom_corpus)
custom_dtm <- DocumentTermMatrix(custom_corpus, control=list(dictionary = fivefreq))
custom_dtm <- apply(custom_dtm, 2, convert_count)
custom_pred <- predict(classifier, newdata=custom_dtm)
custom_pred
table("pred"=custom_pred)
