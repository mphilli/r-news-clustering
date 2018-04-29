# Script for performing document clusting on gathered keywords (k-means)
# Produces k-means 2D plot using PCA

library(readtext)
library(tm)
library(wordcloud)

# directory may require changing

article <- readtext("keywords_all.txt")
d <- strsplit(article$text, '\\s')
art_dir <- file.path(getwd(), "google_articles/")
files <- list.files(path=art_dir, 
                    full.names=TRUE)
print(files)

process_text <- function(corpus_source, process_steps) {
  # return preprocessed corpus object as a Term-Document Matrix
  corpus_obj <- corpus_source  # initialize corpus object
  for (c in process_steps) {
    corpus_obj <- tm_map(corpus_obj, c)
  }
  # Also remove stopwords
  corpus_obj <- tm_map(corpus_obj, removeWords, stopwords('english'))
  corpus_obj <- tm_map(corpus_obj, removeWords, local_stopwords)
  return(corpus_obj)
}
corpus <- Corpus(DirSource(directory=art_dir))

processes <- c(PlainTextDocument, removePunctuation, 
              content_transformer(tolower), stripWhitespace)

article_corpus <- process_text(corpus, processes)
dtm <- DocumentTermMatrix(article_corpus)
findFreqTerms(dtm, lowfreq = 20)
kmeans <- kmeans(dtm, 5)
plot(dtm, col=kmeans$cluster)


# normalize euclidean distance: http://michael.hahsler.net/SMU/CSE7337/install/tm.R
tf_idf <- weightTfIdf(dtm) # apply TF-IDF 
m <- as.matrix(tf_idf) # turn into matrix object
# normalize the data: 
rownames(m) <- 1:nrow(m)
norm_euclid <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_euclid(m)
# apply k
kc <- kmeans(m_norm, 60)

table(kc$cluster)
### show clusters using the first 2 principal components
plot(prcomp(m_norm)$x, col=kc$cl, type="n")
text(prcomp(m_norm)$x, col=kc$cl)
# points(cl$centers, pch=8, cex=2)
library(proxy)
d <- dist(m, method="cosine")
hc <- hclust(d, method="average")
plot(hc)

cl <- cutree(hc, 44)
table(cl)
findFreqTerms(dtm[cl==1], 50)
