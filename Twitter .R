## Packages used 
install.packages("purrr")
install.packages("twitteR")
install.packages("ROAuth")
install.packages("stringr")
install.packages("RCurl")
install.packages("plyr")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
library(wordcloud)
library(purrr)
library(twitteR)
library(dplyr)
library(ROAuth)
library(stringr)
library(RCurl)
library(plyr)
library(tm)
library(SnowballC)
## Sentiment Analysis Function
score.sentiment = function(sentences , pos.words, neg.words , .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(sentences,function(sentence,pos.words,neg.words)
  {
    sentence =gsub('[[:punct:]]',"",sentence)
    sentence =gsub('[[:cntrl:]]',"",sentence)
    sentence =gsub('\\d+','',sentence)
    sentence=tolower(sentence)
    word.list=str_split(sentence,'\\s+')
    words=unlist(word.list)
    pos.matches=match(words,pos.words)
    neg.matches=match(words,neg.words)
    pos.matches= !is.na(pos.matches)
    neg.matches= !is.na(neg.matches)
    score=sum(pos.matches)-sum(neg.matches)
    return(score)
  },pos.words,neg.words,.progress=.progress)
  scores.df=data.frame(scores=scores,text=sentences)
  return(scores.df)
}  
pos.words <- scan('/Users/shauryamalhotra/Desktop/positive.words.txt', what = 'charcter', comment.char = ';')
neg.words <- scan('/Users/shauryamalhotra/Desktop/neg.words.txt', what = 'charcter', comment.char = ';')
bscore <- score.sentiment(tweet_df$text, pos.words, neg.words,.progress = 'text')
rscore <- score.sentiment(tweet2_df$text, pos.words, neg.words, .progress = 'text')
hist(rscore$score)
hist(bscore$score)

## Twitter Keys

consumerKey <- "31Wjtp63DRobyct7E5LdNNe5u"
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/acces_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerSecret <- "Nq936lkHuYTIVgsqOyqyw8SKgZpjjyuBGzm7wdIDMc6b8AHWO5"
accessToken <-"1358292422258692098-ARfSKiUZodOCHegjoz0HXTJqf2rnDl"
accessTokenSecret <- "iQufzVhrDcOABWziGwTpO3tChcbcTb39zknEP3wvsN4tS"


setup_twitter_oauth(consumerKey,consumerSecret, accessToken,accessTokenSecret)
tweet1 <- userTimeline("@TheDemocrats", n=100)
tweet2 <- userTimeline("@GOP", n=100)
tweet_df <- tbl_df(map_df(tweet1, as.data.frame))
tweet2_df <- tbl_df(map_df(tweet2, as.data.frame))





##############
df<- data.frame(matrix(unlist(mycorpus)))

tweet1.df<- twListToDF(tweet1)
mycorpus<- Corpus(VectorSource(tweet1.df$text))
mycorpus<- tm_map(mycorpus, removeWords, stopwords())
removeNumPunct<- function(x) gsub("[a[:alpha:][:space:]]*","",x)
mycorpus<- tm_map(mycorpus, content_transformer(removeNumPunct))
mycorpus<- tm_map(mycorpus, removePunctuation)
mycorpus<- tm_map(mycorpus, content_transformer(tolower))
mycorpus<- tm_map(mycorpus, stripWhitespace)
mycorpus<- tm_map(mycorpus, stemDocument)
dtm <- DocumentTermMatrix(mycorpus)
wordcloud(mycorpus,min.freq = 2)




