#' Calculate G-score (it is chi-squre at the moment)
#' @export
gscore <- function(n_true, n_false, sum_true, sum_false){
  tb <- as.table(rbind(c(n_true, n_false), c(sum_true - n_true, sum_false - n_false)))
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
findNames <- function(tokens, count_min, g=10.83, word_only=TRUE, ...){

  tokens_unlist <- unlist(tokens, use.names = FALSE)
  if(missing(count_min)) count_min <- length(tokens_unlist) / 10 ^ 6 # one in million
  types_upper <- getCasedTypes(tokens_unlist, ...)

  flag <- tokens_unlist %in% types_upper

  cat("Counting capitalized words...\n")
  tb <- table(quanteda::toLower(tokens_unlist), factor(flag, levels=c(TRUE, FALSE)))
  df <- as.data.frame.matrix(tb)
  colnames(df) <- c('upper', 'lower')

  sum_upper <- sum(df$upper)
  sum_lower <- sum(df$lower)
  if(sum_upper==0) stop("All tokens are lowercased. Tokens have to be in original case for name identification.\n")

  cat("Calculating g-score...\n")
  df <- df[df[,1] >= count_min,]
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
selectNames <- function(tokens, selection, padding, ...){

  names <- findNames(tokens, ...)
  cat("Selecting names...\n")
  tokens <- quanteda::selectFeatures2(tokens, names(names), selection, 'fixed',
                                      case_insensitive=FALSE, padding=padding)
  return(tokens)

}

# Idenitfy concatenate sequences of capitalized words. Minimum z-socre is 2.32 (p<0.01) by default.
#' @export
joinNames <- function(tokens, count_min, z=2.32, verbose = FALSE){

  tokens_unlist <- unlist(tokens, use.names = FALSE)
  if(missing(count_min)) count_min <- length(tokens_unlist) / 10 ^ 6 # one in million
  types_upper <- getCasedTypes(tokens_unlist, 'upper')

  cat("Finding sequence of capitalized words...\n")
  seqs <- quanteda::findSequences(tokens, types_upper, count_min=count_min)
  tokens_seqs <- seqs$sequence
  tokens_seqs <- tokens_seqs[order(-seqs$z)] # start joining tokens from the most significant sequences
  tokens_seqs <- tokens_seqs[seqs$z > z]
  cat("Joining capitalized words...\n")
  tokens <- quanteda::joinTokens(tokens, tokens_seqs, verbose=verbose)
  return(tokens)
}

#' Select unique capitalized tokens
#' @export
getCasedTypes <- function(tokens, case='upper'){
  types <- unique(tokens)
  cat("Identifying capitalized words...\n")
  if(case=='upper'){
    types_cased <- types[quanteda::toLower(types) != types]
  }else{
    types_cased <- types[quanteda::toLower(types) == types]
  }
  types_cased <- types_cased[!stringi::stri_detect_regex(types_cased, '^[0-9]')] # exlucde types beging with number
  return(types_cased)
}

#' Stem names identified by selectNames
#' @export
stemNames <- function(pnames, language, len_min=1){
  pnames <- quanteda::toLower(pnames)
  pnames_stem <- quanteda::wordstem(pnames, language) # pattern for proper adjectives
  pnames_stem <- pnames_stem[duplicated(pnames_stem)] # only stems with more than one endings
  pnames_stem <- pnames_stem[stringi::stri_length(pnames_stem) >= len_min]
  pnames_stem_glob <- paste0(unique(pnames_stem), '*')
  return(pnames_stem_glob)
}

#' Select lower or upper-cased words
#' @export
selectCasedFeatures <- function(tokens, case='upper', selection='select', padding=FALSE){
  tokens_unlist <- unlist(tokens, use.names = FALSE)
  types_cased <- getCasedTypes(tokens_unlist, case)
  tokens <- quanteda::selectFeatures2(tokens, types_cased, selection, 'fixed',
                                      case_insensitive=FALSE, padding=padding)
  return(tokens)
}

#' Remove short features
#' @export
removeShortFeatures <- function(tokens, min=3, ...){
  types <- unique(unlist(tokens, use.names = FALSE))
  types_short <- types[stringi::stri_length(types) < min]
  return(quanteda::selectFeatures2(tokens, types_short, selection='remove',
                                   valuetype='fixed', case_insensitive=FALSE, ...))
}

#' Remove padding
#' @export
removePadding <- function(tokens){
  return(quanteda::selectFeatures2(tokens, '', selection='remove', padding=FALSE,
                                   valuetype='fixed', case_insensitive=FALSE))
}
