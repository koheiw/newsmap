#' Read a list of place names from text a file
#' @param file file path to text file
#' @export
readLexicon <- function(file){

  lines <- readLines(file)
  lexicon <- list()
  for(line in lines){
    if(stringi::stri_detect_regex(line, '^#')) next
    values <- unlist(stringi::stri_split_regex(line, '[,;:]'))
    if(length(values) == 1) next
    #print(lexicon)
    lexicon[[values[3]]] <- list(cid=values[1],
                               name=values[2],
                               keywords=values[4:length(values)])
  }
  return(lexicon)
}

#' Extract geograpical keywords from a lexicon as sequence of tokens
#' @param lexicon list of place names
#' @param sep_keyword separator for keywords in lexicon
#' @export
getKeywords <- function(lexicon, sep=' '){
  keywords <- unlist(lapply(lexicon, function(x) stringi::stri_split_regex(x$keywords, sep)), recursive = FALSE)
  return(keywords)
}

#' @name lexicon_en
#' @docType data
#' @title English place names
#' @description Manually compiled list of English place names
#'
NULL

#' @name lexicon_ru
#' @docType data
#' @title Russian place names
#' @description Manually compiled list of Russian place names
#'
NULL
