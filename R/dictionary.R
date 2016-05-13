

#' @export
makeDictionary <- function(tokens, lexicon){

  tokens_unlist <- unlist(tokens, use.names = FALSE)
  len_lexicon <- length(lexicon)
  len_types <- length(unique(tokens_unlist))
  mx_dic <- matrix(rep(NA, len_lexicon * len_types), ncol=len_lexicon, nrow=len_types)
  colnames(mx_dic) <- names(lexicon)
  cat("Scoring", len_types, "features for", len_lexicon, "countries...\n")
  for(code in names(lexicon)){
    country <- lexicon[[code]]
    #cat(country$name, "\n")
    flag <- unlist(lapply(tokens_doc3, function(x, y) rep(any(x %in% y), length(x)), country$keywords_token), use.names=FALSE)
    mx <- as.data.frame.matrix(table(tokens_unlist, factor(flag, levels=c(TRUE, FALSE))))
    if(is.null(rownames(mx_dic))){
      rownames(mx_dic) <- rownames(mx)
    }else{
      if(!all(rownames(mx_dic) == rownames(mx))) stop("Incompatible tokens is given\n")
    }
    smooth <- 0.001
    mx2 <- (mx + smooth) / (colSums(mx) + smooth)
    mx_dic[,code] <- log(mx2[,1]) - log(mx2[,2])
  }
  mx_dic
}
