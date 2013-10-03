# Getting data via twitterR
# 19.9.2013
# Keziah Do

library(ROAuth)
library(twitteR)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(Rcpp)


reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "http://api.twitter.com/oauth/access_token"
authURL <- "http://api.twitter.com/oauth/authorize"
consumerKey <- "BUCzkTa91au7Zy75ZmV4A"
consumerSecret <- "kuSNu0tm4FOSdw01bO0bt4uyjPasKhqoRKdv9rqMZrM"
twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)
getTwitterOAuth(consumerKey, consumerSecret)


mach_tweets = searchTwitter("machine learning", n=500, lang="en")

# create a corpus
mach_corpus = Corpus(VectorSource(mach_text))

# create document term matrix applying some transformations
tdm = TermDocumentMatrix(mach_corpus,
                         control = list(removePunctuation = TRUE,
                                        stopwords = c("marchine", "learning", stopwords("english")),
                                        removeNumbers = TRUE, tolower = TRUE))
# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)
# plot wordcloud
#wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
# save the image in png format
png("MachineLearningCloud.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"),100)
dev.off()