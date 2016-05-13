#' @examples
#' docs <- readLines('/home/kohei/projects/immigration/data/uk_img/2009-2010.txt')
#' sents <- tokenize(docs, what='sentence', simplify = TRUE)
#' tokens <- tokenize(sents, removePunct=TRUE, removeNumbers=TRUE)
#' names <- findNames(tokens, 5)
#'
#' @export
findCapitalWords <- function(tokens, pattern, min=5){

  cat("Identifying capitalized words...\n")
  tb <- table(unlist(tokens, use.names = FALSE))
  flag_upper <- stringi::stri_detect_regex(names(tb), pattern)

  cat("Counting capitalized words...\n")
  # Capitalized words
  df_upper <- data.frame(count=tb[flag_upper])
  df_upper$word <- rownames(df_upper)
  df_upper$match <- quanteda::toLower(df_upper$word)

  cat("Counting uncapitalized words...\n")
  # Other words
  df_lower <- data.frame(count=tb[!flag_upper])
  df_lower$match <- quanteda::toLower(rownames(df_lower))
  df_lower_unique <- aggregate(df_lower$count, by=list(match=df_lower$match), FUN=sum)
  colnames(df_lower_unique) <- c('match', 'count')

  sum_upper <- sum(df_upper$count)
  sum_lower <- sum(df_lower_unique$count)

  df_name <- merge(df_upper[,c('match', 'word', 'count')],
                   df_lower_unique, by="match", all.x=TRUE)
  colnames(df_name) <- c('match', 'word', 'upper', 'lower')
  df_name$upper[is.na(df_name$upper)] <- 0
  df_name$lower[is.na(df_name$lower)] <- 0
  df_name <- df_name[df_name$upper > min,]

  cat("Calculating g-score...\n")
  df_name$gscore <- apply(df_name[,c('upper', 'lower')], 1, function(x, y, z) gscore(x[1], x[2], y, z), sum_upper, sum_lower)
  df_name <- df_name[order(-df_name$gscore),]
  #return(df_name[,c('chisq'),drop=FALSE])
  #list(name=df_name$word, chisq=df_name$chisq)
  gscores <- df_name$gscore
  names(gscores) <- df_name$word
  gscores
}

findNames <- function(x, ...) {
  UseMethod("findNames")
}

#' @export
findNames.character <- function(x){
  
  docs <- x
  sents <- tokenize(docs, what='sentence', simplify = TRUE)
  tokens <- tokenize(sents)
  findNames.tokenizedText(tokens)
  
}

#' @export
findNames.tokenizedText <- function(x){
  
  tokens <- x
  types <- unique(unlist(tokens, use.names = FALSE))
  #pattern <- "^([A-Z]{2,}|[A-Z][0-9]{1,}|[A-Z][a-z\\-]{2,})"
  pattern <- "^[A-Z][A-Za-z0-9\\-]+"
  #pattern <- "^[A-Z]+"
  
  # Select sequence of capitralized words
  types_upper <- types[stringi::stri_detect_regex(types, pattern)]
  mpname <- findSequences(tokens, types_upper, count_min=2)
  tokens2 <- joinTokens(tokens, mpname$sequence[mpname$mue>0], verbose=FALSE)
  
  # Select proper names based frequency of capitalization
  pnames <- findCapitalWords(tokens2, pattern)
  pnames
  
}
