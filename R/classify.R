#' @export
predictCountry <- function(tokens, dict){

  cat("Concatenating multi-part names...\n")
  keywords <- unlist(lapply(colnames(dict), function(x) stringi::stri_split_regex(x, '-')), recursive = FALSE)
  tokens <- joinTokens(tokens, keywords, valueType = 'fixed', verbose = FALSE)
  tokens <- quanteda::selectFeatures2(tokens, colnames(dict))

  mx <- dfm(tokens)
  cols <- intersect(colnames(mx), colnames(dict))
  mx2 <- mx[,cols]
  mx <- mx / rowSums(mx)
  mx3 <- mx2 %*% t(dict[,cols])
  return(mx3)

}



#'
#' @export
getTopCountries <- function(mx, rank=1){
  index_max <- apply(mx, 1, function(x) sort(x, decreasing=TRUE, index.return=TRUE)$ix[rank])
  score_max <- apply(mx, 1, function(x) sort(x, decreasing=TRUE)[rank])
  code_max <- colnames(mx[,index_max])
  return(list(code=code_max,
              score=score_max))
}
