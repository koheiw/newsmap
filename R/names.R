#' Calculate G-score (it is chi-squre at the moment)
gscore <- function(n_true, n_false, sum_true, sum_false, smooth=1){
  tb <- as.table(rbind(c(n_true, n_false), c(sum_true - n_true, sum_false - n_false)))
  tb <- tb + smooth
  suppressWarnings(
    chi <- stats::chisq.test(tb)
  )
  #print(tb)
  #print(chi$expected)
  n_true_exp <- chi$expected[1,1]
  if(n_true > n_true_exp){
    return(unname(chi$statistic))
  }else{
    return(unname(chi$statistic) * -1)
  }
}

#' Identify frequenety capitalized words. Minimum g-socre is 10.83 (p<0.01) by default.
#' @param tokens tokenizedTexts object
#' @param count_min minimum frequency of names
#' @param p p-value for log-likelihood test
#' @param ignore_first ignore capitalization in first words in texts if true
#' @param word_only return only words if true
#' @examples
#' docs <- readLines('/home/kohei/projects/immigration/data/uk_img/2009-2010.txt')
#' sents <- quanteda::tokenize(docs, what='sentence', simplify = TRUE)
#' tokens <- quanteda::tokenize(sents, removePunct=TRUE, removeNumbers=TRUE)
#' names <- findNames(tokens, 5, word_only=FALSE)
#' @export
findNames <- function(tokens, count_min, p=0.001, ignore_first=TRUE, word_only=TRUE){

  tokens_unlist <- unlist(tokens, use.names = FALSE)
  if(missing(count_min)) count_min <- max(2, length(tokens_unlist) / 10 ^ 6) # alt least twice of one in million
  types_upper <- getCasedTypes(tokens_unlist, 'upper')
  flag <- tokens_unlist %in% types_upper

  if(ignore_first){
    first <- c(1, cumsum(lengths(tokens)) + 1)
    first <- first[first <= length(flag)]
    flag[first] <- FALSE
  }

  cat("Counting capitalized words...\n")
  tb <- table(quanteda::toLower(tokens_unlist), factor(flag, levels=c(TRUE, FALSE)))
  df <- as.data.frame.matrix(tb)
  colnames(df) <- c('upper', 'lower')
  df <- df[df$upper >= 1,] # ignore words never appear in uppercase

  sum_upper <- sum(df$upper)
  sum_lower <- sum(df$lower)

  if(sum_upper==0) stop("All tokens are lowercased. Tokens have to be in original case for name identification.\n")

  cat("Calculating g-score...\n")
  g <- stats::qchisq(1 - p, 1) # chisq appariximation to g-score
  df <- df[df$upper >= count_min,]
  df$gscore <- apply(df, 1, function(x, y, z) gscore(x[1], x[2], y, z), sum_upper, sum_lower)
  df <- df[order(-df$gscore),]
  df <- df[df$gscore > g,]

  if(word_only){
    return(rownames(df))
  }else{
    df$p <- 1 - stats::pchisq(df$g, 1)
    return(df)
  }
}

#' Check if words are names by chi-scuqre test
check_name <- function(x, smooth=1){

  n_true <- x[1]
  n_false <- x[2]
  sum_true <- x[3]
  sum_false <- x[4]

  tb <- as.table(rbind(c(n_true, n_false), c(sum_true - n_true, sum_false - n_false)))
  tb <- tb + smooth
  suppressWarnings(
    chi <- stats::chisq.test(tb)
  )
  #print(tb)
  #print(chi$expected)
  n_true_exp <- chi$expected[1,1]
  if(n_true > n_true_exp){
    return(unname(chi$statistic))
  }else{
    return(unname(chi$statistic) * -1)
  }
}


#' Identify frequenety capitalized words. Minimum g-socre is 10.83 (p<0.01) by default.
#' @inheritParams findNames
#' @export
findNames2 <- function(tokens, count_min, p=0.001, word_only=TRUE){

  tokens_unlist <- unlist(tokens, use.names = FALSE)
  if(missing(count_min)) count_min <- max(2, length(tokens_unlist) / 10 ^ 6) # alt least twice of one in million
  tokens_unlist <- tokens_unlist[tokens_unlist != ''] # exlucde padding
  types_upper <- getCasedTypes(tokens_unlist, 'upper')
  flag <- tokens_unlist %in% types_upper

  cat("Counting capitalized words...\n")
  tb <- table(quanteda::toLower(tokens_unlist), factor(flag, levels=c(TRUE, FALSE)))
  df <- as.data.frame.matrix(tb)
  colnames(df) <- c('upper', 'lower')
  df$word <- rownames(df) # rownames are deleted by merge
  df$len <- stringi::stri_length(row.names(df))

  # Calcuate conditional frequency of being uppercase
  df_sum <- aggregate(cbind(df$upper, df$lower), by=list(len=df$len), FUN=sum)
  colnames(df_sum) <- c('len', 'upper_sum', 'lower_sum')

  # Merge to the original df
  df <- merge(df, df_sum, by='len')
  rownames(df) <- df$word
  df <- df[,c('upper', 'lower', 'upper_sum', 'lower_sum', 'len')] # drop word and reorder
  df <- df[df$upper >= 1,] # ignore words never appear in uppercase

  if(sum(df_sum$upper)==0) stop("All tokens are lowercased. Tokens have to be in original case for name identification.\n")

  cat("Performing chi-squre test...\n")
  chisq <- stats::qchisq(1 - p, 1) # chisq appariximation to g-score
  df <- df[df$upper >= count_min,]

  df$chisq <- apply(df[], 1, function(x) check_name(x))
  df <- df[order(-df$chisq),]
  df <- df[df$chisq > chisq,]

  if(word_only){
    return(rownames(df))
  }else{
    df$p <- 1 - stats::pchisq(df$chisq, 1)
    return(df)
  }
}

#' Identify frequenety capitalized words using stopwrods as baseline.
#' @inheritParams findNames
#' @examples
#' df <- readRDS('/home/kohei/Documents/Newsmap2/yahoo-news.RDS')
#' today <- as.Date('2016-02-28')
#' df_recent <- df[today - 7 < df$date & df$date <= today,]
#' docs <- paste0(df_recent$head, ". ", df_recent$body)
#' docs <- cleanTexts(docs)
#' toks <- quanteda::tokenize(docs)
#' df_nam <- findNames3(toks, count_min=5, word_only=FALSE)
#' df_nam[order(df_nam$p),]

#' @export
findNames3 <- function(tokens, count_min, p=0.001, word_only=TRUE, language='english'){

  #tokens <- lapply(tokens, function(x) x[-1]) # remove first words

  tokens_unlist <- unlist(tokens, use.names = FALSE)
  if(missing(count_min)) count_min <- max(2, length(tokens_unlist) / 10 ^ 6) # alt least twice of one in million
  tokens_unlist <- tokens_unlist[tokens_unlist != ''] # exlucde padding
  types_upper <- getCasedTypes(tokens_unlist, 'upper')
  flag <- tokens_unlist %in% types_upper

  cat("Counting capitalized words...\n")
  tb <- table(quanteda::toLower(tokens_unlist), factor(flag, levels=c(TRUE, FALSE)))
  df <- as.data.frame.matrix(tb)
  colnames(df) <- c('upper', 'lower')
  df$word <- rownames(df) # rownames are deleted by merge
  df$none <- df$word %in% stopwords(language) # use stopwords as exmaples of non-names

  # Estimated chance of random capitalization
  df_none <- df[df$none,]
  prob_upper <- sum(df_none$upper) / sum(df_none$upper + df_none$lower)
  #prob_upper <- 0.5
  if(sum(df$upper)==0) stop("All tokens are lowercased. Tokens have to be in original case for name identification.\n")

  cat("Performing test of proportion...\n")
  df <- df[!df$none,] # remove non-names
  df <- df[df$upper >= 1,] # ignore words never appear in uppercase
  df <- df[df$upper >= count_min,]
  suppressWarnings(
  df$p <- apply(df[,c('upper', 'lower')], 1,
                function(x, y) stats::prop.test(x[1], n=x[1] + x[2], p=y, alternative="greater")$p.value,
                prob_upper)
  )
  df <- df[df$p < p,]
  if(word_only){
    return(rownames(df))
  }else{
    return(df)
  }
}

#' Identify frequenety capitalized words with an arbitrary baseline.
#' @inheritParams findNames
#' @export
findNames4 <- function(tokens, count_min, baseline=0.1, p=0.001, word_only=TRUE){

  tokens_unlist <- unlist(tokens, use.names = FALSE)
  if(missing(count_min)) count_min <- max(2, length(tokens_unlist) / 10 ^ 6) # alt least twice of one in million
  tokens_unlist <- tokens_unlist[tokens_unlist != ''] # exlucde padding
  types_upper <- getCasedTypes(tokens_unlist, 'upper')
  flag <- tokens_unlist %in% types_upper

  cat("Counting capitalized words...\n")
  tb <- table(quanteda::toLower(tokens_unlist), factor(flag, levels=c(TRUE, FALSE)))
  df <- as.data.frame.matrix(tb)
  colnames(df) <- c('upper', 'lower')
  df$word <- rownames(df) # rownames are deleted by merge

  if(sum(df$upper)==0) stop("All tokens are lowercased. Tokens have to be in original case for name identification.\n")

  cat("Performing test of proportion...\n")
  df <- df[df$upper >= 1,] # ignore words never appear in uppercase
  df <- df[df$upper >= count_min,]
  suppressWarnings(
    df$p <- apply(df[,c('upper', 'lower')], 1,
                  function(x, y) stats::prop.test(x[1], n=x[1] + x[2], p=y, alternative="greater")$p.value,
                  baseline)
  )
  df <- df[df$p < p,]
  if(word_only){
    return(rownames(df))
  }else{
    return(df)
  }
}

#' Select or remove capitalized words
#' @param tokens tokenizedTexts object
#' @param selection keep or remove features
#' @param padding leave empty tokens if true
#' @param ... passed to quanteda::selectFeatures
#' @export
selectNames <- function(tokens, selection=c('keep', 'remove'), padding=FALSE, ...){

  names <- findNames(tokens, ...)
  types <- unique(unlist(tokens, use.names = FALSE))

  # Selct only upper-rased types
  types_match <- types[toLower(types) %in% toLower(names) &
                       stringi::stri_detect_charclass(types, '\\p{Lu}')]
  tokens <- quanteda::selectFeatures(tokens, types_match, selection, 'fixed',
                                      case_insensitive=FALSE, padding=padding)
  return(tokens)

}

#' Idenitfy concatenate sequences of capitalized words
#' @param tokens tokenizedTexts object
#' @param count_min minimum freqeuency of names
#' @param p p-value for log-likelihood test
#' @param verbose show progress
#' @param types_extra types of tokens that are considered part of names in addition to capitalized words
#' @export
joinNames <- function(tokens, count_min, p=0.001, verbose = FALSE, types_extra){

  tokens_unlist <- unlist(tokens, use.names = FALSE)
  if(missing(count_min)) count_min <- max(2, length(tokens_unlist) / 10 ^ 6) # alt least twice of one in million
  types_upper <- getCasedTypes(tokens_unlist, 'upper')
  if(!missing(types_extra)) types_upper <- c(types_upper, types_extra)
  cat("Finding sequence of capitalized words...\n")

  seqs <- quanteda::findSequences(tokens, types_upper, count_min=count_min)
  seqs$sequence <- seqs$sequence[order(-seqs$z)] # start joining tokens from the most significant sequences
  seqs$p <- seqs$p[order(-seqs$z)]
  cat("Joining capitalized words...\n")
  tokens <- quanteda::joinTokens(tokens, seqs$sequence[seqs$p < p], verbose=verbose)
  return(tokens)
}

#' Extract unique capitalized tokens
#' @param tokens tokenizedTexts object
#' @param case case of words to be extracted
#' @export
getCasedTypes <- function(tokens, case='upper'){
  types <- unique(tokens)
  cat("Identifying capitalized words...\n")
  if(case=='upper'){
    types_cased <- types[stringi::stri_detect_charclass(types, '\\p{Lu}')]
    #types_cased <- types[quanteda::toLower(types) != types]
  }else{
    types_cased <- types[!stringi::stri_detect_charclass(types, '\\p{Lu}')]
    #types_cased <- types[quanteda::toLower(types) == types]
  }
  types_cased <- types_cased[!stringi::stri_detect_regex(types_cased, '^[0-9]')] # exlucde types beging with number
  return(types_cased)
}

#' Stem names identified by selectNames
#' @param names vector of names identifdied by findNames
#' @param language language setting to be passted to the SnowballC stemmer
#' @param len_min minimum length of names to be stemmed
#' @param word_only return only words if true
#' @export
stemNames <- function(names, language='english', len_min=5, word_only=TRUE){

  df <- data.frame(word=quanteda::toLower(names), len=stringi::stri_length(names),
                   stringsAsFactors = FALSE)
  df$stem <- quanteda::wordstem(df$word, language)
  df$dupli <- duplicated(df$stem)
  df$flag <- df$len >= len_min & df$dupli
  df$glob <- ifelse(df$flag, paste0(df$stem, '*'), df$word) # only include long and multi-ending stems
  if(word_only){
    df <- df[df$flag,]
    return(unique(df$glob))
  }else{
    return(df[order(df$word),])
  }
}

#' Select lower or upper-cased words
#' @param tokens tokenizedTexts object
#' @param case case of words to be extracted
#' @param selection keep or remove features
#' @param padding leave empty tokens if true
#' @export
selectCasedFeatures <- function(tokens, case='upper', selection=c('keep', 'remove'), padding=FALSE){
  tokens_unlist <- unlist(tokens, use.names = FALSE)
  types_cased <- getCasedTypes(tokens_unlist, case)
  return(quanteda::selectFeatures(tokens, types_cased, selection=selection, valuetype='fixed',
                                   case_insensitive=FALSE, padding=padding))
}

#' Remove short features
#' @param tokens tokenizedTexts object
#' @param len_min minimum length of tokens to keep
#' @param ... passed to quanteda::selectFeatures
#' @export
removeShortFeatures <- function(tokens, len_min=3, ...){
  types <- unique(unlist(tokens, use.names = FALSE))
  types_match <- types[stringi::stri_length(types) < len_min]
  return(quanteda::selectFeatures(tokens, types_match, selection='remove',
                                   valuetype='fixed', case_insensitive=FALSE, ...))
}

#' Select or remove marks and numbers
#' @param tokens tokenizedTexts object
#' @param numbers remove numeric features if true
#' @param marks remove punctuations and symboles if true
#' @param ... passed to quanteda::selectFeatures
#' @export
removeSpecialFeatures <- function(tokens, number=TRUE, mark=TRUE, net=FALSE, ...){

  types <- unique(unlist(tokens, use.names = FALSE))
  types_match <- c()
  if(number) types_match <- c(types_match, types[stringi::stri_detect_regex(types, '^\\p{N}')])
  if(mark) types_match <- c(types_match, types[stringi::stri_detect_regex(types, '^(\\p{P}|\\p{S})+$')])
  if(net) types_match <- c(types_match, types[stringi::stri_detect_regex(types, '^(http|ftp|www)|@|#')])
  return(quanteda::selectFeatures(tokens, types_match, selection='remove',
                                   valuetype='fixed', case_insensitive=FALSE, ...))
}

#' Remove padding
#' @param tokens tokenizedTexts object
#' @export
removePadding <- function(tokens){
  return(quanteda::selectFeatures(tokens, '', selection='remove', padding=FALSE,
                                   valuetype='fixed', case_insensitive=FALSE))
}


#' @export
cleanTexts <- function(text, lang='english'){
  if(lang=='english'){
    text <- stringi::stri_replace_all_fixed(text, "'s", '') # posession
    text <- stringi::stri_replace_all_fixed(text, ".", '') # acronyms
  }
  return(text)
}
