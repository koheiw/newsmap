rm(list = ls())

#' @export
compileLexicon <- function(file_txt, file_rdata){

  if(missing(file_txt)) file_txt="data/countries_en.txt"
  if(missing(file_rds)) file_rds=paste0(strsplit(file_txt, "\\.")[[1]][[1]], '.RData')

  lines <- readLines(file_txt)
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
  #saveRDS(lexicon, file_rds)
  save(lexicon, file=file_rdata)
}
