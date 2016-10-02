#' Predict geographical association of texts
#' @param tokens tokenizedTexts
#' @param dict geigraphical dicitonary created by makeGeoDictionary
#' @export
predictCountry <- function(tokens, dict, lang='english'){

  cat("Concatenating multi-part names...\n")
  dict <- as(dict, 'denseMatrix')
  keywords <- unlist(lapply(colnames(dict), function(x) stringi::stri_split_regex(x, '-')), recursive = FALSE)
  tokens <- joinTokens(tokens, keywords, valuetype = 'fixed', verbose = FALSE)
  tokens <- quanteda::selectFeatures(tokens, colnames(dict))

  mx <- quanteda::dfm(tokens, verbose=FALSE)
  #colnames(mx) <- cleanTokens(colnames(mx), lang)
  #mx <- quanteda::compress(mx) # merge equivalent columns

  cols <- intersect(colnames(mx), colnames(dict))
  mx2 <- mx[,cols]
  mx2 <- mx2 / Matrix::rowSums(mx2)
  #mx2 <- mx2 / sums
  mx3 <- mx2 %*% Matrix::t(dict[,cols])
  return(mx3)

}

#' Extract most strongly assosiated countries
#' @param pred prediciton by predictCountry
#' @param rank rank of country
#' @export
getTopCountries <- function(pred, rank=1){
  mx <- pred
  index_max <- apply(mx, 1, function(x) sort(x, decreasing=TRUE, index.return=TRUE)$ix[rank])
  score_max <- apply(mx, 1, function(x) sort(x, decreasing=TRUE)[rank])
  code_max <- colnames(mx[,index_max])
  return(list(code=code_max,
              score=score_max))
}

#' Evaluate classification perfromance by precision and recall
#' @param class_true vector of true classes
#' @param class_test vercor of predicted classes
#' @export
performance <- function(class_true, class_test){

  df <- data.frame(true=ifelse(is.na(class_true), '', class_true), 
                   test=ifelse(is.na(class_test), '', class_test))
  df <- df[!is.na(df$true) & df$true != '',] # remove unknown in true class

  classes <- unique(df$true)
  mx <- matrix(ncol=6, nrow=0)
  for(class in classes){
    #cat(class, "\n")
    n <- sum(df$true == class)
    tp <- sum(df$true == class & df$test == class)
    fp <- sum(df$true != class & df$test == class)
    fn <- sum(df$true == class & df$test != class)
    precision <- tp / (tp + fp)
    recall <- tp / (tp + fn)
    #print(paste(n, tp, fp, fn, precision, recall))
    mx <- rbind(mx, c(n, tp, fp, fn, precision, recall))
  }
  mode(mx) <- 'numeric'
  rownames(mx) <- classes
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

