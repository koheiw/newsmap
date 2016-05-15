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
findNames <- function(tokens, count_min=5, g=10.83, ...){

  tokens_unlist <- unlist(tokens, use.names = FALSE)
  types_upper <- getCapitalTypes(tokens_unlist, ...)

  flag <- tokens_unlist %in% types_upper
  tb <- table(toLower(tokens_unlist), factor(flag, levels=c(TRUE, FALSE)))
  df <- as.data.frame.matrix(tb)
  colnames(df) <- c('upper', 'lower')

  sum_upper <- sum(df$upper)
  sum_lower <- sum(df$lower)
  if(sum_upper==0) stop("All tokens are lowercased. Tokens have to be in original case for name identification.\n")

  cat("Calculating g-score...\n")
  df <- df[df$upper > count_min,]
  df$gscore <- apply(df[,c('upper', 'lower')], 1, function(x, y, z) gscore(x[1], x[2], y, z), sum_upper, sum_lower)
  df <- df[order(-df$gscore),]
  df <- df[df$gscore > g,]

  gscore <- df$gscore
  names(gscore) <- toUpper(rownames(df))
  return(gscore)
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
joinNames <- function(tokens, count_min=5, z=2.32, verbose = FALSE, ...){

  tokens_unlist <- unlist(tokens, use.names = FALSE)
  types_upper <- getCapitalTypes(tokens_unlist, regex, ignore, ...)

  cat("Finding sequence of capitalized words...\n")
  seqs <- quanteda::findSequences(tokens, types_upper, count_min=count_min)

  seqs_signif <- seqs$sequence[seqs$z < z]
  cat("Joining capitalized words...\n")
  tokens <- joinTokens(tokens, seqs_signif, verbose=verbose)
  return(tokens)
}

#' Select unique capitalized tokens
#' @export
getCapitalTypes <- function(tokens, regex, ignore){

  if(missing(ignore)) ignore <- c()

  types <- unique(tokens)
  types <- types[!types %in% ignore] # exclude types to ignore

  cat("Identifying capitalized words...\n")
  if(missing(regex)){
    types_upper <- types[tolower(types) != types]
  }else{
    types_upper <- types[stringi::stri_detect_regex(types, regex)]
  }

  types_upper <- types_upper[!stringi::stri_detect_regex(types_upper, '^[0-9]')] # exlucde types beging with number
  return(types_upper)
}
