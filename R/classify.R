#' Predict geographical association of news stories
#' @export
predictCountry <- function(tokens, dict){

  cat("Concatenating multi-part names...\n")
  keywords <- unlist(lapply(colnames(dict), function(x) stringi::stri_split_regex(x, '-')), recursive = FALSE)
  tokens <- joinTokens(tokens, keywords, valueType = 'fixed', verbose = FALSE)
  tokens <- quanteda::selectFeatures2(tokens, colnames(dict))

  mx <- dfm(tokens, verbose=FALSE)
  #sums <- Matrix::rowSums(mx)
  cols <- intersect(colnames(mx), colnames(dict))
  mx2 <- mx[,cols]
  mx2 <- mx2 / Matrix::rowSums(mx2)
  #mx2 <- mx2 / sums
  mx3 <- mx2 %*% t(dict[,cols])
  return(mx3)

}

#' Find most strongly assosiated countries
#' @export
getTopCountries <- function(mx, rank=1){
  index_max <- apply(mx, 1, function(x) sort(x, decreasing=TRUE, index.return=TRUE)$ix[rank])
  score_max <- apply(mx, 1, function(x) sort(x, decreasing=TRUE)[rank])
  code_max <- colnames(mx[,index_max])
  return(list(code=code_max,
              score=score_max))
}

#' Evaluate classification perfromance by precision and recall
#' @export
performance <- function(class_true, class_test){

  # Remove unknown items
  flag_unkwnow <- is.na(class_true) | class_true == ''
  class_true <- class_true[!flag_unkwnow]
  class_test <- class_test[!flag_unkwnow]

  class_all <- unique(class_true)
  #return(class_all)
  mx <- matrix(ncol=6, nrow=0)
  for(class_temp in class_all){
    print(class_temp)
    n <- sum(class_true == class_temp)
    tp <- sum(class_true == class_temp & class_test == class_temp)
    fp <- sum(class_true != class_temp & class_test == class_temp)
    fn <- sum(class_true == class_temp & class_test != class_temp)
    precision <- tp / (tp + fp)
    recall <- tp / (tp + fn)
    #print(paste(n, tp, fp, fn, precision, recall))
    mx <- rbind(mx, c(n, tp, fp, fn, precision, recall))
  }
  mode(mx) <- 'numeric'
  rownames(mx) <- class_all
  colnames(mx) <- c('n', 'tp', 'fp', 'fn', 'precision', 'recall')

  #Micro-average of precision = (TP1+TP2)/(TP1+TP2+FP1+FP2)
  p <- sum(mx[,'tp'], na.rm=T) / sum(mx[,c('tp', 'fp')])
  #Micro-average of recall = (TP1+TP2)/(TP1+TP2+FN1+FN2)
  r <- sum(mx[,'tp'], na.rm=T) / sum(mx[,c('tp', 'fn')])
  #Macro-average precision = (P1+P2)/2
  P <- sum(mx[,'precision'], na.rm=T) / nrow(mx)
  #Macro-average recall = (R1+R2)/2
  R <- sum(mx[,'recall'], na.rm=T) / nrow(mx)

  return(list(p=p, r=r, P=P, R=R))
}

