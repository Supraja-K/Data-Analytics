library("ROAuth")
library("twitteR")
library("RCurl")
library("plyr")
library("stringr")
library("dplyr")
consumerKey='vUC75Ska6PC8xsa3aZL9m7pjH'
consumerSecret='EaF9THE0E5CD8zRVPoWYg9hAkOYzvoQpc5sDLIujba9fyn5euX'
accessToken='1137950459698634753-frltbHe4SdR0WgxC10AP1yyK3trw6M'
accessTokenSecret='dpMYszVKZxDkH35Xa1zIRotVfinLHHvR6PUPSSiKwDtco'
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)
score.sentiment = function(tweets, pos.words, neg.words)
  
{
  
  require(plyr)
  require(stringr)
  
  scores = laply(tweets, function(tweet, pos.words, neg.words) {
    
    
    
    tweet = gsub('https://','',tweet) 
    tweet = gsub('http://','',tweet) 
    tweet=gsub('[^[:graph:]]', ' ',tweet) 
    tweet = gsub('[[:punct:]]', '', tweet) 
    tweet = gsub('[[:cntrl:]]', '', tweet) 
    tweet = gsub('\\d+', '', tweet) 
    tweet=str_replace_all(tweet,"[^[:graph:]]", " ") 
    
    tweet = tolower(tweet) 
    
    word.list = str_split(tweet, '\\s+') 
    
    words = unlist(word.list) 
    
    pos.matches = match(words, pos.words) 
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches) 
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches) 
    return(score)
  }, pos.words, neg.words )
  
  scores.df = data.frame(score=scores, text=tweets)
  return(scores.df)
}

tweets = searchTwitter('VIT',n=100)
Tweets.text = lapply(tweets,function(t)t$getText())
pos <- scan('C:/Users/Keerthivasan/Downloads/positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
neg <- scan('C:/Users/Keerthivasan/Downloads/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
analysis = score.sentiment(Tweets.text, pos, neg)

analysis$very.pos = as.numeric(analysis$score > 0)
analysis$very.neg = as.numeric(analysis$score < 0)
analysis$very.neu = as.numeric(analysis$score == 0)
numpos = sum(analysis$very.pos)
numneg = sum(analysis$very.neg)
numneu = sum(analysis$very.neu)

s<- c(numpos,numneg,numneu)
lbls <-c("POSITIVE","NEGATIVE","NEUTRAL")
pct <- round(s/sum(s)*100)
lbls <- paste(lbls, pct)
lbls <-paste(lbls,"%",sep="")

pie(pct,labels = lbls, col = rainbow(length(lbls)),main="OPINION - VIT")

