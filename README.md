
# Newsmap: geographical document classifier

<!-- badges: start -->

[![CRAN
Version](https://www.r-pkg.org/badges/version/newsmap)](https://CRAN.R-project.org/package=newsmap)
[![Downloads](https://cranlogs.r-pkg.org/badges/newsmap)](https://CRAN.R-project.org/package=newsmap)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/newsmap?color=orange)](https://CRAN.R-project.org/package=newsmap)
[![R build
status](https://github.com/koheiw/newsmap/workflows/R-CMD-check/badge.svg)](https://github.com/koheiw/newsmap/actions)
[![codecov](https://codecov.io/gh/koheiw/newsmap/branch/master/graph/badge.svg)](https://codecov.io/gh/koheiw/newsmap)
<!-- badges: end -->

Semi-supervised Bayesian model for geographical document classification.
Newsmap automatically constructs a large geographical dictionary from a
corpus to accurate classify documents. Currently, the **newsmap**
package contains seed dictionaries in multiple languages that include
*English*, *German*, *French*, *Spanish*, *Portuguese*, *Russian*,
*Italian*, *Hebrew*, *Arabic*, *Japanese*, *Chinese*.

The detail of the algorithm is explained in [Newsmap: semi-supervised
approach to geographical news
classification](https://www.tandfonline.com/eprint/dDeyUTBrhxBSSkHPn5uB/full).
**newsmap** has also been used in scientific research in various fields
([Google
Scholar](https://scholar.google.com/scholar?oi=bibs&hl=en&cites=3438152153062747083)).

## How to install

**newsmap** is available on CRAN since the version 0.6. You can install
the package using R Studio GUI or the command.

``` r
install.packages("newsmap")
```

If you want to the latest version, please install by running this
command in R. You need to have **devtools** installed beforehand.

``` r
install.packages("devtools")
devtools::install_github("koheiw/newsmap")
```

## Example

In this example, using a text analysis package
[**quanteda**](https://quanteda.io) for preprocessing of textual data,
we train a geographical classification model on a [corpus of news
summaries collected from Yahoo
News](https://www.dropbox.com/s/e19kslwhuu9yc2z/yahoo-news.RDS?dl=1) via
RSS in 2014.

### Download example data

``` r
download.file('https://www.dropbox.com/s/e19kslwhuu9yc2z/yahoo-news.RDS?dl=1', '~/yahoo-news.RDS')
```

### Train Newsmap classifier

``` r
require(newsmap)
## Loading required package: newsmap
require(quanteda)
## Loading required package: quanteda
## Package version: 3.2.1
## Unicode version: 13.0
## ICU version: 66.1
## Parallel computing: 6 of 6 threads used.
## See https://quanteda.io for tutorials and examples.

# Load data
dat <- readRDS('~/yahoo-news.RDS')
dat$text <- paste0(dat$head, ". ", dat$body)
dat$body <- NULL
corp <- corpus(dat, text_field = 'text')

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

# quanteda v1.5 introduced 'nested_scope' to reduce ambiguity in dictionary lookup
toks_label <- tokens_lookup(toks, data_dictionary_newsmap_en, 
                            levels = 3, nested_scope = "dictionary")
dfmt_label <- dfm(toks_label)

dfmt_feat <- dfm(toks, tolower = FALSE)
dfmt_feat <- dfm_select(dfmt_feat, selection = "keep", '^[A-Z][A-Za-z1-2]+', 
                        valuetype = 'regex', case_insensitive = FALSE) # include only proper nouns to model
dfmt_feat <- dfm_trim(dfmt_feat, min_termfreq = 10)

model <- textmodel_newsmap(dfmt_feat, dfmt_label)

# Features with largest weights
coef(model, n = 7)[c("us", "gb", "fr", "br", "jp")]
## $us
## WASHINGTON Washington         US  Americans       YORK     States        NYC 
##  10.032798   9.498110   8.788495   8.235961   6.952463   6.286699   6.125502 
## 
## $gb
##    London    LONDON   Britain Britain's        UK   British    Briton 
## 10.654624 10.648244 10.397478  9.754731  9.711822  7.846117  7.534455 
## 
## $fr
##    France     PARIS     Paris    French     Valls Frenchman    CANNES 
## 11.322852 10.449551 10.260602  8.165804  8.005718  7.838664  7.743354 
## 
## $br
##    Brazil Brazilian       SAO     PAULO       RIO   JANEIRO       Rio 
##  11.63404  10.33499  10.28737  10.28284  10.21235  10.20551  10.09797 
## 
## $jp
##     Japan  Japanese     TOKYO     Tokyo       Abe     Abe's    Shinzo 
## 11.744956 10.939229 10.796363 10.100190  8.654381  8.066166  7.984406
```

### Predict geographical focus of texts

``` r
pred_data <- data.frame(text = as.character(sub_corp), country = predict(model))
```

|        | text                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | country |
|:-------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:--------|
| text63 | ’08 French champ Ivanovic loses to Safarova in 3rd. PARIS (AP) — Former French Open champion Ana Ivanovic lost in the third round Saturday, beaten 6-3, 6-3 by 23rd-seeded Lucie Safarova of the Czech Republic.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         | fr      |
| text68 | Up to $1,000 a day to care for child migrants. More than 57,000 unaccompanied children, mostly from Central America, have been caught entering the country illegally since last October, and President Barack Obama has asked for $3.7 billion in emergency funding to address what he has called an “urgent humanitarian solution.” “One of the figures that sticks in everybody’s mind is we’re paying about $250 to $1,000 per child,” Senator Jeff Flake told reporters, citing figures presented at a closed-door briefing by Homeland Security Secretary Jeh Johnson. Federal authorities are struggling to find more cost-effective housing, medical care, counseling and legal services for the undocumented minors. The base cost per bed was $250 per day, including other services, Senator Dianne Feinstein said, without providing details. | us      |
| text69 | 1,000 DRC ex-rebels break out of Uganda camp: army. About 1,000 former fighters from a former Democratic Republic of Congo rebel group broke out Tuesday from a camp where they being held in Uganda just as soldiers were about to repatriate them, the Ugandan army said. “A thousand rebels from the M23 (group) have escaped” from the camp in Bihanga, about 300 kilometres (190 miles) southwest of the Ugandan capital Kampala, a spokesman for the Ugandan army said on the official Twitter account.                                                                                                                                                                                                                                                                                                                                            | ug      |
| text73 | 1,000 killed in Boko Haram conflict this year. More than 1,000 people have been killed so far this year in three states in northeastern Nigeria worst hit by Boko Haram violence, according to the country’s main relief organisation. The National Emergency Management Agency (NEMA) figures are the starkest indication yet of the increase in bloodshed in Borno, Adamawa and Yobe that have caused growing concern. NEMA said in a presentation in Abuja on Tuesday that people living in the states were “caught up in an intensifying conflict”, which has been raging since 2009. Violence has increased in northeastern Nigeria since the new year, including a high-profile attack on a boarding school in Yobe, which saw dozens of students slaughtered in their beds.                                                                       | ng      |
| text78 | 1,000 migrants repulsed at Spanish border. MADRID (AP) — Officials say around 1,000 migrants of sub-Saharan origin have failed in an attempt to get over Spain’s three-tier barbed-wire border fence separating its North African enclave of Melilla from Morocco in a bid to enter Europe.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | es      |
| text79 | Some 1,000 migrants try to reach Spain from Africa. MADRID (AP) — Some 700 migrants stormed a border fence to try to enter Spain’s northwest African enclave city of Melilla from Morocco on Tuesday while the sea rescue service said it had picked up some 500 others trying to enter the country clandestinely by crossing the Strait of Gibraltar in boats, officials said.                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | es      |
