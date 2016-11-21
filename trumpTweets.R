library(twitteR)
library(tm)
library(qdap)
library(wordcloud)
library(RCurl)
library(httr)
library(devtools)

# Twitter Authentication --------------------------------------


setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# Functions ---------------------------------------------------

# Function to clean corpus
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "Trump", "trump", "amp"))
  corpus <- tm_map(corpus, content_transformer(tolower))
  return(corpus)
}

# Function for bigram token maker
bigram.tokenizer <- function(x)
   unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

# Additional Settings -----------------------------------------

# Set so text strings will not be factors of categories
options(stringsAsFactors = FALSE) 

# Prevent errors with tweets in different languages
Sys.setlocale('LC_ALL','C')

# Analysis ----------------------------------------------------

# Gather Tweets
trumpTweets <- searchTwitter("#Trump", since = "2016-11-18", until = "2016-11-19", n = 1000)

# Set up tweets as a dataframe, and reduce to only id & text
df.trumpTweets <- do.call("rbind", lapply(trumpTweets, as.data.frame))
df.reduced.trumpTweets <- data.frame(id = df.trumpTweets$id, text = df.trumpTweets$text)
custom.reader <- readTabular(mapping=list(content="text", id="id"))
corpus.trumpTweets <- VCorpus(DataframeSource(df.reduced.trumpTweets), readerControl=list(reader=custom.reader))

corpus.trumpTweets <- clean_corpus(corpus.trumpTweets)

# Set up Term Document Matrix & get freqency of bigram terms
tdm.trumpTweets <- TermDocumentMatrix(corpus.trumpTweets, control = list(tokenize = bigram.tokenizer))
tdm.m.trumpTweets <- as.matrix(tdm.trumpTweets)

tdm.freq.trumpTweets <- sort(rowSums(tdm.m.trumpTweets), decreasing = TRUE)
tdm.df.trumpTweets <- data.frame(word = names(tdm.freq.trumpTweets), freq = tdm.freq.trumpTweets)

display.brewer.all()
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]

set.seed(1)
wordcloud(tdm.df.trumpTweets$word,tdm.df.trumpTweets$freq,max.words=50, random.order=FALSE, colors=pal)

