#' Predict geographical association of texts
#' @param tokens tokenizedTexts
#' @param dict geigraphical dicitonary created by makeGeoDictionary
#' @export
predict_country <- function(toks, dict, lang='english'){

    mx_dict <- as(dict, 'denseMatrix')

    cat("Concatenating multi-word names...\n")
    toks <- quanteda::tokens_compound(toks, colnames(dict), valuetype = "fixed",
                                      case_insensitive = TRUE, concatenator = ' ')
    mx <- quanteda::dfm(toks, tolower = FALSE)
    mx <- quanteda::dfm_select(mx, "^[A-Z]", valuetype = 'regex', case_insensitive = FALSE)
    mx <- quanteda::dfm_toupper(mx)

    feats <- intersect(colnames(mx), colnames(mx_dict))
    mx <- mx[,feats]
    mx_dict <- mx_dict[,feats]
    mx <- quanteda::dfm_weight(mx, 'relFreq')

    mx2 <- mx %*% Matrix::t(mx_dict)
    flag_drop <- apply(mx2, 2, function(x) all(x==0)) # check if all words are zero
    mx2 <- mx2[,!flag_drop]
    attr(mx2, 'tokens') <- quanteda::tokens_toupper(toks)
    attr(mx2, 'features') <- feats
    return(mx2)

}

#' Extract most strongly assosiated countries
#' @param pred prediciton by predictCountry
#' @param rank rank of country
#' @export
get_country <- function(pred, rank=c(1, 2)){
    country <- t(apply(pred, 1, function(x) names(sort(x, decreasing=TRUE))[rank]))
    colnames(country) <- paste0('country', rank)
    score <- t(apply(pred, 1, function(x) sort(unname(x), decreasing=TRUE)[rank]))
    colnames(score) <- paste0('score', rank)
    df <- cbind(as.data.frame(country, stringsAsFactors = FALSE),
                as.data.frame(score))
    return(df)
}

#' @export
show_country <- function(pred, rank=c(1, 2), range){

    mx <- matrix(rep(0, length(rank) * nrow(pred)), nrow=nrow(pred))
    for (i in 1:nrow(pred)){
        if (i %in% range) {
            cat(i, names(toks[i]), "-----------------------\n")
            top <- sort(pred[i,], decreasing = TRUE)[rank]
            cat(paste0(names(top), ": ", round(top, 3)), "\n")
            cat('Entries\n')
            print(intersect(pred@tokens[[i]], pred@features))
            cat('Tokens\n')
            print(pred@tokens[[i]])
            cat("\n")
        }
    }
}

#' Evaluate classification accuracy in precision and recall
#' @param class_true vector of true classes
#' @param class_test vercor of predicted classes
#' @export
accuracy <- function(class_true, class_test){

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

