# Newsmap
Newsmap is developed to classify news stories according to their geographical focus. Its [online version](http://example.com "International Newsmap") has been working since 2011. It has been in Python, but recently implemented in R. This program automatically construct a large geographical dictionary from a corpus of news stories for accurate classification.

## How to install
This package is not upload to CRAN, so please install by running this command in R. You need to have devtools installed beforehand.
```
devtools::install_github("koheiw/Newsmap")
```

## Example
In the following example, newsmap creates a dicitonary for June 21 2012 with news stories collected from Yahoo News via RSS. Yahoo News stories are not in this package as it is too large, but availabe on request.
```
# Load data
load('data/countries_en.RData')
df <- readRDS('../Newsmap/yahoo-news.RDS')

# Specify capitalized words to ignore
month <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
day <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
pattern <- "^[A-Z][A-Za-z0-9\\-]+"
keywords <- getKeywords(lexicon)

# Extract news from last 7 days
today = as.Date('2012-07-21')
df_recent <- df[today - 7 < df$date & df$date <= today,]
docs <- paste0(df_recent$head, ". ", df_recent$body)

# Tokenize text
tokens <- tokenize(docs, removePunct = FALSE)
tokens <- selectFeatures2(tokens, c(month, day), selection='remove', padding=TRUE)
tokens <- selectFeatures2(tokens, stopwords(), selection='remove', padding=FALSE)

# Join multi-part names
tokens <- joinTokens(tokens, keywords, valueType = 'glob', verbose = FALSE)
tokens <- joinSequence(tokens, pattern, verbose = FALSE)

# Idenntify names and drop others
pnames <- findNames(tokens, pattern, count_min=2)
tokens2 <- selectFeatures2(toUpper(tokens), names(pnames), selection='keep', case_insensitive = FALSE)

# Make dictionary
dict <- makeDictionary(tokens2, lexicon)

```
