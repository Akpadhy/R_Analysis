#==================================================#
#==================================================#
      #Sentiment Analysis on Demonetisation#
#==================================================#
#==================================================#
install.packages("twitteR")
install.packages("bit")
install.packages("bit64")
install.packages("rjson")
install.packages("ROAuth")
install.packages("RCurl")
install.packages("bitops")
install.packages("digest")
install.packages("sentiment")
install.packages("tm")
install.packages("NLP")
install.packages("slam")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("ggplot2")
==================================================
library(RColorBrewer)
library(bitops)
library(NLP)
library(twitteR)
library(tm)
library(SnowballC)
library(wordcloud)
library(RCurl)
library(ROAuth)
library(stringr)
library(ggplot2)

tweets.df <- read.csv("/Users/padhya/Documents/Python/My_Practice/Demonetization_Analysis/demonetization-tweets.csv")
str(tweets.df)
tweets <- as.character(tweets.df$text)
sentiment.score <- function(sentences, positive.words, negative.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores <- laply(sentences, function(sentence, positive.words, negative.words)
  {
    
    ## clean up sentences with R's regex-driven global substitute, gsub():
    
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    
    # remove retweets
    sentence <- gsub('(RT|via)((?:\\b\\W*@\\W+)+)', '', sentence)
    
    # remove at people
    sentence <- gsub('@\\w+', '', sentence)
    
    # remove punctuations
    sentence <- gsub('[[:punct:]]', '', sentence)
    
    # remove numbers
    sentence <- gsub('[[:digit:]]', '', sentence)
    
    # remove html links
    sentence <- gsub('http[s]?\\w+', '', sentence)
    
    # remove extra spaces
    sentence <- gsub('[ \t]{2,}', '', sentence)
    sentence <- gsub('^\\s+|\\s+$', '', sentence)
    
    # removing NA's
    sentence <- sentence[!is.na(sentence)]
    
    # convert to lower case:
    sentence <- tolower(sentence)
    
    # split into words. str_split is in the stringr package
    
    word.list <- str_split(sentence, '\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    
    words <- unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    
    negative.matches <- match(words, negative.words)
    positive.matches <- match(words, positive.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    
    positive.matches <- !is.na(positive.matches)
    negative.matches <- !is.na(negative.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    
    score <- sum(positive.matches) - sum(negative.matches)
    
    return(score)
  }, positive.words, negative.words, .progress=.progress )
  
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

positive <- scan("/Users/padhya/nltk_data/corpora/opinion_lexicon/positive-words.txt", what= "character", comment.char= ";")
negative <- scan("/Users/padhya/nltk_data/corpora/opinion_lexicon/negative-words.txt", what= "character", comment.char= ";")

tweets.analysis <- sentiment.score(tweets, positive, negative, .progress="none")

str(tweets.analysis)
tweets.analysis$sentiment[tweets.analysis$score == 0] <- "Neutral" 
tweets.analysis$sentiment[tweets.analysis$score < 0] <- "Negative"
tweets.analysis$sentiment[tweets.analysis$score > 0] <- "Positive"

tweets.analysis$sentiment <- factor(tweets.analysis$sentiment)
table(tweets.analysis$score)
mean(tweets.analysis$score) # slighlty positive
median(tweets.analysis$score)
summary(tweets.analysis$sentiment)

ggplot(data = tweets.analysis, aes(x = score, fill = sentiment)) + 
  geom_bar() + 
  labs(title = "Sentiment Score Bar Plot", x = "Sentiment Score", y = "Tweet Count") +
  scale_x_continuous(breaks = seq(-6,6,1)) + 
  scale_y_continuous(breaks = seq(0,4000,500)) + 
  scale_fill_manual(guide = guide_legend("Sentiment"), values = c("#DD0426","#246EB9","#04B430"))

bag <- tweets.df$text
bag <- Corpus(VectorSource(bag))
bag <- tm_map(bag, tolower)
bag <- tm_map(bag, PlainTextDocument)
bag <- tm_map(bag, removePunctuation)
bag <- tm_map(bag, removeWords, c("demonetization","demonetisation", stopwords("english")))
bag <- tm_map(bag, stripWhitespace)
bag <- tm_map(bag, stemDocument)

wordcloud(bag, min.freq = 50,
          max.words=1500, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"), scale = c(3,0.5))