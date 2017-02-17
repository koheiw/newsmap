# Newsmap
Newsmap is developed to classify news stories according to their geographical focus. Its [online version](http://example.com "International Newsmap") has been working since 2011. It has been in Python, but recently implemented in R. This program automatically construct a large geographical dictionary from a corpus of news stories for accurate classification.

## How to install
This package is not upload to CRAN, so please install by running this command in R. You need to have devtools installed beforehand.
```
devtools::install_github("koheiw/Newsmap")
```

## Example
In the following example, newsmap creates a dicitonary for June 21 2012 with news stories collected from Yahoo News via RSS. Yahoo News stories are not in this package as it is too large, but availabe on request.

### Construct classifier 
```r
library(Newsmap)
library(quanteda)

# Load data
df <- readRDS('../Newsmap/yahoo-news.RDS')
df$text <- paste0(df$head, ". ", df$body)
df$body <- NULL
corp <- corpus(df, text_field = 'text')

# Custom stopwords
month <- c('January', 'February', 'March', 'April', 'May', 'June',
           'July', 'August', 'September', 'October', 'November', 'December')
day <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
agency <- c('AP', 'AFP', 'Reuters')

# Select training period
corp_sub <- corpus_subset(corp, '2014-01-01' <= date & date <= '2014-12-31')

# Tokenize
toks <- tokens(corp_sub)
toks <- tokens_remove(toks, stopwords('english'), valuetype = 'fixed', padding = TRUE)
toks <- tokens_remove(toks, c(month, day, agency), valuetype = 'fixed', padding = TRUE)

# Construct classifier from seed dictionary
seed <- dictionary(file=system.file("data", "english.yml", package = "Newsmap"))
model <- construct(toks, seed, min_count_seq = 10)

```

### Apply classifier 
```r
# Load your data
txts2 <- readlines('my_texts.txt')
toks2 <- tokens(txts2)

# Classify
pred <- predict_country(tokens_remove(toks2, stopwords('english')), model)
get_country(pred)

```
