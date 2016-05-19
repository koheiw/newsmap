#' Convert the format of a lexicon of place names from text to RData
#' @examples
#' lexicon_en <- readLexicon('/home/kohei/projects/newsmap/countries.conf')
#' save(lexicon_en, file='data/lexicon_en.RData')
#' lexicon_ru <- readLexicon('/home/kohei/projects/newsmap/countries_ru.conf')
#' save(lexicon_ru, file='data/lexicon_ru.RData')
#'
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
#' @export
getKeywords <- function(lexicon, sep=' '){
  keywords <- unlist(lapply(lexicon, function(x) stringi::stri_split_regex(x$keywords, sep)), recursive = FALSE)
  return(keywords)
}
