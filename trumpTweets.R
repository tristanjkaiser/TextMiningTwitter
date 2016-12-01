# Tristan Kaiser
# 11/28/2017
# Next Steps:
# Use dplyr & functions to reduce code
library(plyr)
library(twitteR)
library(tm)
library(qdap)
library(wordcloud)
library(RCurl)
library(httr)
library(devtools)
library(RColorBrewer)
library(purrr)
library(ggplot2)
library(ggthemes)
install.packages("ggthemes")
# Twitter Authentication --------------------------------------


api_key <- 'CRDsansBY3AzDrfDSjl4YeR9m'
api_secret <- 'mjpQq0Tk4VDgukoYBhlFqIP4uCbsRbiNjtV6qoT4jTyRJp24oE'
access_token <- '1477661306-cCgdZ2BkN7f0u5UCt7R4yJ6zcdhLPbQ6bLbY48G'
access_token_secret <-'g3VOz6tv0WR0wgSC09VsBErKBCmRWOWAH0O3VykZDHgxK'
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# Functions ---------------------------------------------------

# Function to clean corpus

clean_corpus <- function(corpus){
  corpus <- corpus %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, c(stopwords("en"), "trump", "amp"))
  return(corpus)
}

# Additional Settings -----------------------------------------

# Set so text strings will not be factors of categories
options(stringsAsFactors = FALSE) 

# Prevent errors with tweets in different languages
Sys.setlocale('LC_ALL','C')

# Analysis ----------------------------------------------------
custom.reader <- readTabular(mapping=list(content="text", id="id"))

# Function to gather tweets & convert to data frame
gather_tweets <- function(searchTerm, dateStart, dateEnd, maxTweets)
{
  tweets <- searchTwitter(searchTerm, since = dateStart, until = dateEnd, n = maxTweets)
  return(do.call("rbind",lapply(tweets,as.data.frame)))
}

# Get the tweets
trumpTweets <- gather_tweets("Trump", "2016-11-26", "2016-11-27", 1000)
hillaryTweets <- gather_tweets("Hillary", "2016-11-26", "2016-11-27", 1000)

# Reduce and create corpus
trumpTweets <- data.frame(id = trumpTweets$id, text = trumpTweets$text)
hillaryTweets <- data.frame(id = hillaryTweets$id, text = hillaryTweets$text)

corpus.trumpTweets <- VCorpus(DataframeSource(trumpTweets), readerControl=list(reader=custom.reader))
corpus.hillaryTweets <- VCorpus(DataframeSource(hillaryTweets), readerControl=list(reader=custom.reader))

# Clean the corpus
corpus.trumpTweets <- clean_corpus(corpus.trumpTweets)
corpus.hillaryTweets <- clean_corpus(corpus.hillaryTweets)

# Set up Term Document Matrix
tdm.trumpTweets <- TermDocumentMatrix(corpus.trumpTweets)
tdm.trumpTweets <- as.matrix(tdm.trumpTweets)

tdm.hillaryTweets <- TermDocumentMatrix(corpus.hillaryTweets)
tdm.hillaryTweets <- as.matrix(tdm.hillaryTweets)

freq.trumpTweets <- sort(rowSums(tdm.trumpTweets), decreasing = TRUE)
explore.trumpTweets <- data.frame(word = names(freq.trumpTweets), frequency = freq.trumpTweets)

freq.hillaryTweets <- sort(rowSums(tdm.hillaryTweets), decreasing = TRUE)
wc.hillaryTweets <- data.frame(word = names(freq.hillaryTweets), freq = freq.hillaryTweets)

# Choose colors for Wordcloud
display.brewer.all()
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]

# Wordcloud of tweets
set.seed(1)
wordcloud(wc.hillaryTweets$word,wc.trumpTweets$freq,max.words=50, random.order=FALSE, colors=pal)
