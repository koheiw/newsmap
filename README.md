# Newsmap
Geographical news classifier

## How to install
devtools::install_github("koheiw/Newsmap")

### Load data

```
load('data/countries_en.RData')
df <- readRDS('/home/kohei/Documents/Newsmap/yahoo-news.RDS')
```

### Specify capitalized words to ignore
```
month <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
day <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
pattern <- "^[A-Z][A-Za-z0-9\\-]+"
keywords <- getKeywords(lexicon)

today = as.Date('2012-07-21')
df_recent <- df[today - 7 < df$date & df$date <= today,]
docs <- paste0(df_recent$head, ". ", df_recent$body)
tokens <- tokenize(docs, removePunct = FALSE)
tokens <- selectFeatures2(tokens, c(month, day), selection='remove', padding=TRUE)
tokens <- selectFeatures2(tokens, stopwords(), selection='remove', padding=FALSE)




```
