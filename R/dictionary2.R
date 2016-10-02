
#' Construct a geographical dictionary from a list of place names
#' @param tokens tokenizedTexts object
#' @param lexicon list of place names
#' @param smooth smoother for coditional frequency
#' @param sep_keyword separator for keywords in lexicon
#' @export
makeGeoDictionary2 <- function(tokens, lexicon, power=1, smooth=0.001, sep=' '){

  if(length(lexicon) == 0) stop("Lexicon is empty")
  mx <- quanteda::dfm(tokens, verbose=FALSE)
  mx <- mx[,order(colnames(mx))]
  mx_dic <- matrix(rep(0, ncol(mx) * length(lexicon)), ncol=ncol(mx), nrow=length(lexicon)) # create empty dictionary
  colnames(mx_dic) <- colnames(mx)
  rownames(mx_dic) <- names(lexicon)

  cat("Scoring", ncol(mx_dic), "features for", nrow(mx_dic), "countries...\n")
  for(code in names(lexicon)){
    country <- lexicon[[code]]
    #cat(country$name, "\n")

    glob <- stringi::stri_replace_all_fixed(country$keywords, sep, '-') # make keywords into tokens
    regex <- paste(utils::glob2rx(glob), collapse='|')
    flag_feature <- stringi::stri_detect_regex(colnames(mx), regex, case_insensitive=TRUE)
    if(!any(flag_feature)) next # skip when no keyword match

    flag_document <- quanteda::rowSums(mx[,flag_feature, drop=FALSE]) > 0 # glags for documents that containe keywords
    mx2 <- rbind(quanteda::colSums(mx[flag_document,,drop=FALSE]), quanteda::colSums(mx[!flag_document,,drop=FALSE]))
    flag_zero <- mx2[1,] == 0 # flag words do not occur

    # Scoring words
    mx3 <- ((mx2 ^ power) + smooth) / (Matrix::rowSums(mx2) + smooth) # conditional likelihood
    lr <- log(mx3[1,]) - log(mx3[2,]) # # calculate likelihood-ratio
    lr[flag_zero] <- 0 # fill with zero if words do no occur
    mx_dic[code,] <- lr
  }
  flag_drop <- apply(mx_dic, 1, function(x) all(x==0)) # check if all words are zero
  mx_dic <- mx_dic[!flag_drop,]
  mx_dic <- mx_dic[order(rownames(mx_dic)),]
  #print(dimnames(mx_dic))
  return(as(mx_dic, "sparseMatrix"))
}

#' @export
topEntries <-function(dict, code, n=20){
  ents <- dict[code,]
  ents <- ents[order(ents, decreasing = TRUE)]
  print(head(ents, n))
  cat(sum(ents > 0), 'non-zero entries\n')
}

