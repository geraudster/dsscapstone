
## Loading data

source('loadData.R')

#loadDataFile()

en_US.blogs <- loadData('en_US', 'blogs', 5000)
en_US.news <- loadData('en_US', 'news', 5000)
en_US.twitter <- loadData('en_US', 'twitter', 5000)

# Some stats

lengths <- sapply(list(en_US.blogs, en_US.news, en_US.twitter), length)
sizes <- format(sapply(list(en_US.blogs, en_US.news, en_US.twitter), function(x) {
  format(object.size(x), units = 'auto')}))
names(lengths) <- c('en_US.blogs', 'en_US.news', 'en_US.twitter')
rbind(lengths, sizes)

library(tm)
library(SnowballC)
t <- iconv(en_US.blogs, from = 'UTF-8', to="utf-8-mac")
myCorpus <- Corpus(VectorSource(en_US.blogs), readerControl = list(language = 'en'))
myCorpus.2 <- tm_map(myCorpus, stripWhitespace)
myCorpus.2 <- tm_map(myCorpus.2, removePunctuation)
myCorpus.2 <- tm_map(myCorpus.2, content_transformer(tolower))
myCorpus.2 <- tm_map(myCorpus.2, removeWords, stopwords("english"))
myCorpus.2 <- tm_map(myCorpus.2, stripWhitespace)
myCorpus.2 <- tm_map(myCorpus.2, stemDocument, language = meta(myCorpus.2, "language"))
myCorpus.3 <- tm_filter(myCorpus.2, function (x) {
  length(unlist(strsplit(stringr::str_trim(x$content), '[[:blank:]]+'))) > 1
  })

library(devtools)
install_github('wrathematics/ngram')
bigramTokenizer <- function(x) {
  x <- as.character(x)
  
  # Find words
  one.list <- c()
  tryCatch({
    one.gram <- ngram::ngram(x, n = 1)
    one.list <- ngram::get.ngrams(one.gram)
  }, 
  error = function(cond) { warning(cond) })
  
  # Find 2-grams
  two.list <- c()
  tryCatch({
    two.gram <- ngram::ngram(x, n = 2)
    two.list <- ngram::get.ngrams(two.gram)
  },
  error = function(cond) { warning(cond) })
  
  res <- unlist(c(one.list, two.list))
  res[res != '']
}

dtm <- DocumentTermMatrix(myCorpus.3[1:10])

dtm <- DocumentTermMatrix(myCorpus.3, control = list(tokenize = bigramTokenizer))
dtmTest <- lapply(myCorpus.3, bigramTokenizer)

en_US.blogs.df <- data.frame(type = 'blogs',
                             document = en_US.blogs,
                             length = sapply(en_US.blogs, nchar),
                             row.names = NULL,
                             stringsAsFactors = FALSE)
en_US.news.df <- data.frame(type = 'news',
                            document = en_US.news,
                            length = sapply(en_US.news, nchar),
                            row.names = NULL,
                            stringsAsFactors = FALSE)
en_US.twitter.df <- data.frame(type = 'twitter',
                               document = en_US.twitter,
                               length = sapply(en_US.twitter, nchar),
                               row.names = NULL,
                               stringsAsFactors = FALSE)


sampleSize <- 1000
en_US.blogs.df.sample <- en_US.blogs.df[sample(en_US.blogs.df$document, sampleSize),]
en_US.news.df.sample <- en_US.news.df[sample(en_US.news.df$document, sampleSize),]
en_US.twitter.df.sample <- en_US.twitter.df[sample(en_US.twitter.df$document, sampleSize),]

en_US.all.sample <- rbind(en_US.blogs.df.sample,
                          en_US.news.df.sample,
                          en_US.twitter.df.sample)

library(ggplot2)
qplot(en_US.all.sample$type,
      log(en_US.all.sample$length),
      fill = en_US.all.sample$type,
      geom = 'boxplot')

contains <- function(pattern, word) {
  length(grep(pattern, word)) == 1
} 

analyzeString <- function(x) {
  tags <- c()
  if(contains('[[:alnum:]]+', x)) {
    tags <- c(tags, 'word')
  }

  if(contains('[[:digit:]]+', x)) {
    tags <- c(tags, 'digit')
  }
  
  if(contains('[[:punct:]]+', x)) {
    tags <- c(tags, 'punct')
  }
  tags
}

tokenizeDocument <- function(doc) {
  sapply(unlist(strsplit(doc, '[[:blank:]]')),
         analyzeString)
}

