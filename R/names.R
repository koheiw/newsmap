#' Calculate G-score (it is chi-squre at the moment)
#' @export
gscore <- function(n_true, n_false, sum_true, sum_false, smooth=1){
  tb <- as.table(rbind(c(n_true, n_false), c(sum_true - n_true, sum_false - n_false)))
  tb <- tb + smooth
  suppressWarnings(
    chi <- chisq.test(tb)
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
#' @examples
#' docs <- readLines('/home/kohei/projects/immigration/data/uk_img/2009-2010.txt')
#' sents <- tokenize(docs, what='sentence', simplify = TRUE)
#' tokens <- tokenize(sents, removePunct=TRUE, removeNumbers=TRUE)
#' names <- findNames(tokens, 5)
#'
#' @export
findNames <- function(tokens, count_min, p=0.001, word_only=TRUE){

  tokens_unlist <- unlist(tokens, use.names = FALSE)
  if(missing(count_min)) count_min <- max(2, length(unlist(tokens)) / 10 ^ 6) # alt least twice of one in million
  types_upper <- getCasedTypes(tokens_unlist, 'upper')

  flag <- tokens_unlist %in% types_upper

  cat("Counting capitalized words...\n")
  tb <- table(quanteda::toLower(tokens_unlist), factor(flag, levels=c(TRUE, FALSE)))
  df <- as.data.frame.matrix(tb)
  colnames(df) <- c('upper', 'lower')
  df <- df[df$upper >= 1,] # ignore words never appear in uppercase

  sum_upper <- sum(df$upper)
  sum_lower <- sum(df$lower)

  if(sum_upper==0) stop("All tokens are lowercased. Tokens have to be in original case for name identification.\n")

  cat("Calculating g-score...\n")
  g <- qchisq(1 - p, 1) # chisq appariximation to g-score
  df <- df[df$upper >= count_min,]
  df$gscore <- apply(df, 1, function(x, y, z) gscore(x[1], x[2], y, z), sum_upper, sum_lower)
  df <- df[order(-df$gscore),]
  df <- df[df$gscore > g,]

  if(word_only){
    return(rownames(df))
  }else{
    return(df)
  }
}

# Select or remove names
#' @export
selectNames <- function(tokens, selection='keep', padding=FALSE, ...){

  names <- findNames(tokens, ...)
  types <- unique(unlist(tokens, use.names = FALSE))

  # Selct only upper-rased types
  types_match <- types[toLower(types) %in% toLower(names) &
                       stringi::stri_detect_charclass(types, '\\p{Lu}')]
  tokens <- quanteda::selectFeatures2(tokens, types_match, selection, 'fixed',
                                      case_insensitive=FALSE, padding=padding)
  return(tokens)

}

# Idenitfy concatenate sequences of capitalized words.
#' @export
joinNames <- function(tokens, count_min, p=0.001, verbose = FALSE){

  tokens_unlist <- unlist(tokens, use.names = FALSE)
  if(missing(count_min)) count_min <- max(2, length(unlist(tokens)) / 10 ^ 6) # alt least twice of one in million
  types_upper <- getCasedTypes(tokens_unlist, 'upper')

  cat("Finding sequence of capitalized words...\n")

  seqs <- quanteda::findSequences(tokens, types_upper, count_min=count_min)
  seqs$sequence <- seqs$sequence[order(-seqs$z)] # start joining tokens from the most significant sequences
  seqs$p  <- seqs$p[order(-seqs$z)]
  cat("Joining capitalized words...\n")
  tokens <- quanteda::joinTokens(tokens, seqs$sequence[seqs$p < p], verbose=verbose)
  return(tokens)
}

#' Select unique capitalized tokens
#' @export
getCasedTypes <- function(tokens, case='upper'){
  types <- unique(tokens)
  cat("Identifying capitalized words...\n")
  if(case=='upper'){
    types_cased <- types[stringi::stri_detect_charclass(types, '\\p{Lu}')]
    #types_cased <- types[quanteda::toLower(types) != types]
  }else{
    types_cased <- types[stringi::stri_detect_charclass(types, '\\p{Ll}')]
    #types_cased <- types[quanteda::toLower(types) == types]
  }
  types_cased <- types_cased[!stringi::stri_detect_regex(types_cased, '^[0-9]')] # exlucde types beging with number
  return(types_cased)
}

#' Stem names identified by selectNames
#' @export
stemNames <- function(names, language='en', len_min=5, word_only=TRUE){

  df <- data.frame(word=quanteda::toLower(names), len=stringi::stri_length(names),
                   stringsAsFactors = FALSE)
  df$stem <- quanteda::wordstem(df$word, language)
  df$dupli <- duplicated(df$stem)
  df$stem <- df$len >= len_min & df$dupli
  df$glob <- ifelse(df$stem, paste0(df$stem, '*'), df$word) # only include long and multi-ending stems
  if(word_only){
    df <- df[df$stem,]
    return(unique(df$glob))
  }else{
    return(df[order(df$word),])
  }
}

#' Select lower or upper-cased words
#' @export
selectCasedFeatures <- function(tokens, case='upper', selection='select', padding=FALSE){
  tokens_unlist <- unlist(tokens, use.names = FALSE)
  types_cased <- getCasedTypes(tokens_unlist, case)
  tokens <- quanteda::selectFeatures2(tokens, types_cased, selection=selection, valuetype='fixed',
                                      case_insensitive=FALSE, padding=padding)
  return(tokens)
}

#' Remove short features
#' @export
removeShortFeatures <- function(tokens, len_min=3, ...){
  types <- unique(unlist(tokens, use.names = FALSE))
  types_match <- types[stringi::stri_length(types) < len_min]
  return(quanteda::selectFeatures2(tokens, types_match, selection='remove',
                                   valuetype='fixed', case_insensitive=FALSE, ...))
}

# Select or remove marks and numbers
#' @export
removeSpecialFeatures <- function(tokens, numbers=TRUE, marks=TRUE, ...){

  types <- unique(unlist(tokens, use.names = FALSE))
  types_match <- c()
  if(numbers) types_match <- c(types_match, types[stringi::stri_detect_regex(types, '^\\p{N}')])
  if(marks) types_match <- c(types_match, types[stringi::stri_detect_regex(types, '^(\\p{P}|\\p{S})+$')])
  #if(net) types_match <- c(types_match, types[stringi::stri_detect_regex(types, '^(http|ftp|www)|@|#')])
  return(quanteda::selectFeatures2(tokens, types_match, selection='remove',
                                   valuetype='fixed', case_insensitive=FALSE, ...))
  return(tokens)
}

#' Remove padding
#' @export
removePadding <- function(tokens){
  return(quanteda::selectFeatures2(tokens, '', selection='remove', padding=FALSE,
                                   valuetype='fixed', case_insensitive=FALSE))
}
