# Script for generating word clouds, creating basket data model, and performing association rules

library(readtext)
library(tm)
library(wordcloud)


Sys.getlocale()
options(encoding = "ascii")
art_dir <- file.path(getwd(), "google_articles/")

# custom news article stopwords
local_stopwords <- c("advertisement", "continue", "said", "dont", "says", "didnt", "show", "say", "page", "hes",
                     "just", "much", "get", "see", "know", "cbs", "well", "really", "yeah", "news", "hide", "caption",
                     "can", "one", "like", "people", "photos", "video", "videos", "reading", "main", "story", "thats",
                     "pinterest", "facebook", "share", "skip", "close", "also", "photo", "best", "friday", "walmart", 
                     "amazon", "will", "back", "going", "think", "asked", "time", "life", "dhs", "cbp", "report", 
                     "reportedly", "ago", "reported")

text_mining <- function(article) {
  # returns a dataframe of the article tokens, sorted by frequency 
  process_text <- function(inner, process_steps) {
    # return preprocessed corpus object as a Term-Document Matrix
    tm_article <- Corpus(VectorSource(inner$text))  # initialize corpus object
    for (c in process_steps) {
      tm_article <- tm_map(tm_article, c)
    }
    # Also remove stopwords
    tm_article <- tm_map(tm_article, removeWords, stopwords('english'))
    tm_article <- tm_map(tm_article, removeWords, local_stopwords)
    return(tm_article)
  }
  
  processes <- c(PlainTextDocument, removePunctuation, 
                content_transformer(tolower), stripWhitespace)
  article_corpus <- process_text(article, processes) 
  dtm <- TermDocumentMatrix(article_corpus)
  m <- as.matrix(dtm) # create matrix object
  v <- sort(rowSums(m), decreasing=TRUE) # sort by most frequent
  d <- data.frame(word = names(v), freq=v) # create dataframe object 
  return(d)
}

# wordcloud function; generate a wordcloud from most frequent tokens of each article
gen_wordcloud <- function(dataframe) {
  wordcloud(words = dataframe$word, freq = dataframe$freq, min.freq = 1, 
            max.words = 200, random.order = FALSE, rot.per = 0.35,
            colors=brewer.pal(8, "Dark2"))
  }

gen_data_frame <- function (art_files, wordclouds=FALSE) {
  df = data.frame(ID=numeric(), items=character())
  i <- 0
  for (f in art_files) {
    print(f) # file name
    article <- readtext(f)
    tm_article <- text_mining(article)
    keywords <- subset(tm_article, tm_article$freq > 2) # collect only words appearing more than 4 times
    if (wordclouds == TRUE) {
      # produce some wordclouds every so often
      if ((i %% 13) == 0) {
        gen_wordcloud(tm_article)
      }
    }

    key_vect <- as.vector(keywords$word)
    if (length(key_vect) > 22) { # trim total number of keywords down a bit
      key_vect <- key_vect[1:20]
    } 
    i <- i + 1
    # create a dataframe consisting of the most frequent words 
    df <- rbind(df, data.frame(TID=i, items=paste(key_vect, collapse = " ")))
  }
  return(df)
}

assoc_rules <- function(df_in) { 
  detach(package:tm, unload=TRUE)
  keywords_path = "keywords_new.csv"
  write.csv(df_in, file = keywords_path, row.names=FALSE, quote=FALSE)
  ar_data <- read.transactions(keywords_path, format=c("basket"), sep=" ", quote = "\"'")
  summary(ar_data)
  model <- apriori(ar_data, parameter=list(support=0.007, confidence=0.7, minlen=3))
  summary(model)
  inspect(sort(model, decreasing=TRUE, by="support")[1:100])
}

files <- dir(art_dir, pattern="*.txt", full.names = TRUE)
df <- gen_data_frame(files)
assoc_rules(df)


