
Sys.getenv('conkey')
library(twitteR)

consumerKey <- Sys.getenv('conkey')
consumerSecret <- Sys.getenv('consec')
accessToken <- Sys.getenv('acct')
accessTokenSecret <- Sys.getenv('accts')

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)
#Ytweets_geolocated <- searchTwitter("#GST", n=1000, lang="en",
                                   #geocode = "28.4595,77.0266,500mi", 
                                   #since = "2018-01-04")
#tweets_geolocated.df <- twListToDF(Ytweets_geolocated)
#tweets_geolocated.df[1:4,1:5]
#colnames(tweets_geolocated.df)

tweets <- searchTwitter("#GST", n=300, lang="en",
                        #geocode = "28.459497,77.026638,500mi", 
                        since = "2018-01-04") 
# top 300 tweets that contain search term

tweet_txt = lapply(tweets, function(x) x$getText())
tweet_txt1= twListToDF(tweets)
tweet_txt2 = tweet_txt1[,"text"]

head(tweet_txt)

clean <- function(sometxt)
{
  sometxt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", sometxt)
  sometxt = gsub("@\\w+", "", sometxt)
  sometxt = gsub("[[:punct:]]", "", sometxt)
  sometxt = gsub("[[:digit:]]", "", sometxt)
  sometxt = gsub("http\\w+", "", sometxt)
  sometxt = gsub("[ \t]{2,}", "", sometxt)
  sometxt = gsub("^\\s+|\\s+$", "", sometxt)
  sometxt = gsub("amp", "", sometxt)
  # define "tolower error handling" function
  trylower = function(x)
  {
    y = NA
    tryerror = tryCatch(tolower(x), error=function(e) e)
    tryerror
    if (!inherits(tryerror, "error"))
      y = tolower(x)
    return(y)
  }
  
  sometxt = sapply(sometxt, trylower)
  sometxt = sometxt[sometxt != ""]
  names(sometxt) = NULL
  sometxt <- strsplit(sometxt," ")
  return(sometxt)
}

tweet_clean = clean(tweet_txt)

head(tweet_clean,5)

setwd("D:/R datasets")
positive=scan('positive-words.txt',what='character',comment.char=';')
negative=scan('negative-words.txt',what='character',comment.char=';')


positive=c(positive,"helpfull")

negative=negative[negative!="helpfull"]

# calculate the positive score function
returnpscore=function(t) {
  pos.match=match(t,positive)
  pos.match=!is.na(pos.match)
  pos.score=sum(pos.match)
  return(pos.score)
}
positive.score=lapply(tweet_clean,function(x) returnpscore(x))

head(positive.score)
pcount=0
for (i in 1:length(positive.score)) {
  pcount=pcount+positive.score[[i]]
}
pcount

#for counting the negative matching words.
returnnscore=function(twet) {
  neg.match=match(twet,negative)
  neg.match=!is.na(neg.match)
  neg.score=sum(neg.match)
  return(neg.score)
}
negative.score=lapply(tweet_clean,function(x) returnnscore(x))
negative.score
ncount=0
for (i in 1:length(negative.score)) {
  ncount=ncount+negative.score[[i]]
}
ncount
#####list### return the number of positive words in the tweet
poswords=function(t){
  pmatch=match(t,positive)
  posw=positive[pmatch]
  posw=posw[!is.na(posw)]
  return(posw)
}

negwords=function(t){
  nmatch=match(t,negative)
  negw=negative[nmatch]
  negw=negw[!is.na(negw)]
  return(negw)
}

words=NULL
pdatamart=data.frame(words)

for (t in tweet_clean) {
  pdatamart=c(poswords(t),pdatamart)
}
head(pdatamart,10)

words=NULL
ndatamart=data.frame(words)

for (t in tweet_clean) {
  ndatamart=c(negwords(t),ndatamart)
}
head(ndatamart,10)

pwords <- unlist(pdatamart)
nwords <- unlist(ndatamart)
pwords
nwords
dpwords=data.frame(table(pwords))
dnwords=data.frame(table(nwords))
dpwords
dnwords

library(dplyr)
dpwords = dpwords%>%
  mutate(pwords=as.character(pwords))%>%
  filter(Freq>3)%>%print

## negative 
dnwords = dnwords%>%
  mutate(nwords=as.character(nwords))%>%
  filter(Freq>10)%>%print


library(ggplot2)
ggplot(dpwords,aes(pwords,Freq))+
  geom_bar(stat="identity",fill="lightblue")+
  theme_bw()+
  geom_text(aes(pwords,Freq,label=Freq),size=4)+
  labs(x="Major Positive Words", y="Frequency of Occurence",
       title=paste("Major Positive Words and Occurence in \n '","GST","' twitter feeds, n =",300))+
  geom_text(aes(2,50,label=paste("Total Positive Words :",pcount)),size=4)+
  theme(axis.text.x=element_text(angle=30))

## negative plot

ggplot(dnwords,aes(nwords,Freq))+
  geom_bar(stat="identity",fill="lightblue")+
  theme_bw()+
  geom_text(aes(nwords,Freq,label=Freq),size=4)+
  labs(x="Major Negative Words", y="Frequency of Occurence",
       title=paste("Major Negative Words and Occurence in \n '","GST","' twitter feeds, n =",300))+
  geom_text(aes(1,50,label=paste("Total Negative Words :",ncount)),size=4)+
  theme(axis.text.x=element_text(angle=30))
