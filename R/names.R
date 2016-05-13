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

#' @export
findKeywords <- function(lexicon, tokens){
  
  types <- unique(unlist(tokens, use.names = FALSE))
  for(code in names(lexicon)){
    country <- lexicon[[code]]
    cat(country$name, "\n")
    mpnames <- list()
    for(keyword in country$keywords){
      parts <- stringi::stri_split_regex(keyword, ' ')
      #cat("parts--------------------\n")
      #print(parts)
      if(stringi::stri_detect_fixed(keyword, '*')){
        regex <- unlist(lapply(parts, utils::glob2rx))
        #cat("regex--------------------\n")
        #print(regex)
        match <- lapply(regex, function(x, y) y[stringi::stri_detect_regex(y, x)], types)
        #cat("match--------------------\n")
        #print(match)
        #print(length(match))
        if(length(unlist(regex)) != length(match)) next
        args <- c(match, stringsAsFactors=FALSE)
        match_comb <- do.call(expand.grid, args) # Produce all possible combinations
        #cat("match_comb----------------------\n")
        #print(match_comb)
        mpnames <- unname(c(mpnames, split_df_cpp(t(match_comb))))
        #cat("mpnames----------------------\n")
        #print(mpnames)
        
      }else{
        #cat("parts----------------------\n")
        #print(parts)
        mpnames <- unname(c(mpnames, parts))
      }
    }
    lexicon[[code]][['keywords_seq']] <- mpnames
    lexicon[[code]][['keywords_token']] <- sapply(mpnames, paste, collapse = "-")
  }
  lexicon
}
