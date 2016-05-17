
#' Construct a large geographical dictionary from place names
#' @export
makeGeoDictionary <- function(tokens, lexicon, power=1, smooth=0.001, sep=' '){

  tokens_unlist <- unlist(tokens, use.names = FALSE)
  len_lexicon <- length(lexicon)
  if(len_lexicon == 0) stop("Lexicon is empty")
  types <- unique(tokens_unlist)
  len_types <- length(types)
  if(len_types == 0) stop("Text length is zero")

  # Create dictionary
  mx_dic <- matrix(rep(0, len_lexicon * len_types), ncol=len_types, nrow=len_lexicon) # create empty dictionary
  rownames(mx_dic) <- names(lexicon)
  cat("Scoring", len_types, "features for", len_lexicon, "countries...\n")
  for(code in names(lexicon)){
    country <- lexicon[[code]]
    #cat(country$name, "\n")
    regex <- utils::glob2rx(stringi::stri_replace_all_fixed(country$keywords, sep, '-')) # make keywords into tokens
    types_match <- types[stringi::stri_detect_regex(types, paste0(regex, collapse='|'), case_insensitive = TRUE)] # search the keywords
    flag <- unlist(lapply(tokens, function(x, y) rep(any(x %in% y), length(x)), types_match), use.names=FALSE) # flag all the words in the text
    if(sum(flag) == 0) next # skip when no keyword match
    mx <- t(as.data.frame.matrix(table(tokens_unlist, factor(flag, levels=c(TRUE, FALSE))))) # make matrix where columns are countries

    if(is.null(colnames(mx_dic))){
      colnames(mx_dic) <- colnames(mx)
    }else{
      if(!all(colnames(mx_dic) == colnames(mx))) stop("Incompatible tokens is given\n")
    }
    mx2 <- (mx + smooth) / (rowSums(mx) + smooth)
    mx_dic[code,] <- log(mx2[1,] ^ power) - log(mx2[2,]) # insert into the empty dictionary
  }
  mx_dic <- mx_dic[order(rownames(mx_dic)),]
  return(mx_dic)
}

