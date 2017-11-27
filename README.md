
Newsmap
=======

Newsmap is developed to classify news stories according to their geographical focus. Its [online version](http://example.com "International Newsmap") has been working since 2011. It has been in Python, but recently implemented in R. This program automatically construct a large geographical dictionary from a corpus of news stories for accurate classification.

How to install
--------------

This package is not upload to CRAN, so please install by running this command in R. You need to have devtools installed beforehand.

``` r
devtools::install_github("koheiw/newsmap")
```

Example
-------

In the following example, newsmap creates a dicitonary for June 21 2012 with news stories collected from Yahoo News via RSS. Yahoo News stories are not in this package as it is too large, but availabe on request.

### Download example data

``` r
download.file('https://www.dropbox.com/s/e19kslwhuu9yc2z/yahoo-news.RDS?dl=1', '~/yahoo-news.RDS')
```

### Train Newsmap classifier

``` r
library(newsmap)
library(quanteda)
## quanteda version 0.99.9025
## Using 7 of 8 threads for parallel computing
## 
## Attaching package: 'quanteda'
## The following object is masked from 'package:utils':
## 
##     View

# Load data
data <- readRDS('~/yahoo-news.RDS')
data$text <- paste0(data$head, ". ", data$body)
data$body <- NULL
corp <- corpus(data, text_field = 'text')

# Custom stopwords
month <- c('January', 'February', 'March', 'April', 'May', 'June',
           'July', 'August', 'September', 'October', 'November', 'December')
day <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
agency <- c('AP', 'AFP', 'Reuters')

# Select training period
sub_corp <- corpus_subset(corp, '2014-01-01' <= date & date <= '2014-12-31')

# Tokenize
toks <- tokens(sub_corp)
toks <- tokens_remove(toks, stopwords('english'), valuetype = 'fixed', padding = TRUE)
toks <- tokens_remove(toks, c(month, day, agency), valuetype = 'fixed', padding = TRUE)

# Traing model using seed dictionary
data('data_dictionary_newsmap_en')
label_toks <- tokens_lookup(toks, data_dictionary_newsmap_en, levels = 3) # level 3 is countries
label_dfm <- dfm(label_toks)

feat_dfm <- dfm(toks, tolower = FALSE)
feat_dfm <- dfm_select(feat_dfm, '^[A-Z][A-Za-z1-2]+', valuetype = 'regex', case_insensitive = FALSE) # include only proper nouns to model
feat_dfm <- dfm_trim(feat_dfm, min_count = 10)

model <- textmodel_newsmap(feat_dfm, label_dfm)
summary(model, n = 15)
## Classes:
##    bi, km, dj, er, et, ke, mg, mw, mu, yt, mz, re, rw, sc, so ...  
## Features:
##    French, Ivanovic, Safarova, PARIS, Former, Open, Ana, Lucie, Czech, Republic, Central, America, President, Barack, Obama ...  
## Documents:
##    text63, text68, text69, text73, text78, text79, text84, text85, text86, text92, text94, text103, text104, text106, text115 ...
```

### Predict geographical focus of texts

``` r
country <- predict(model)
head(country)
## text63 text68 text69 text73 text78 text79 
##   "fr"   "us"   "ug"   "ng"   "es"   "es"
head(sort(table(country), decreasing = TRUE))
## country
##   gb   us   ru   ua   au   cn 
## 9819 8071 7848 7021 5854 5630
```
