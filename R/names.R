#' @export
gscore <- function(col, non, sum_col, sum_non){
  tb <- as.table(rbind(c(col, non), c(sum_col - col, sum_non - non)))
  suppressWarnings(
    chi <- chisq.test(tb)
  )
  #print(tb)
  #print(chi$expected)
  col_exp <- chi$expected[1,1]
  if(col > col_exp){
    return(unname(chi$statistic))
  }else{
    return(unname(chi$statistic) * -1)
  }
}

#' @examples
#' docs <- readLines('/home/kohei/projects/immigration/data/uk_img/2009-2010.txt')
#' sents <- tokenize(docs, what='sentence', simplify = TRUE)
#' tokens <- tokenize(sents, removePunct=TRUE, removeNumbers=TRUE)
#' names <- findNames(tokens, 5)
#'
#' @export
findNames <- function(tokens, pattern, ignore, count_min=5){

  if(missing(ignore)) ignore <- c()

  cat("Identifying capitalized words...\n")
  #tb <- table(unlist(tokens, use.names = FALSE))
  tokens_unlist <- unlist(tokens, use.names = FALSE)
  types <- unique(tokens_unlist)
  types <- types[!types %in% ignore]
  types_upper <- types[stringi::stri_detect_regex(types, pattern)]


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
  df <- df[df$gscore > 10.83,]

  gscore <- df$gscore
  names(gscore) <- toUpper(rownames(df))
  return(gscore)
}


#' @export
joinSequence <- function(tokens, pattern, verbose = FALSE){
  # Idenitfy sequences of capitalized words
  types <- unique(unlist(tokens, use.names = FALSE))
  types_upper <- types[stringi::stri_detect_regex(types, pattern)]
  seqs <- findSequences(tokens, types_upper, count_min=2)
  seqs_signif <- seqs$sequence[seqs$z < 2.32] # p=0.01
  cat("Joining multi-part names\n")
  tokens <- joinTokens(tokens, seqs_signif, verbose=verbose)
  return(tokens)
}
