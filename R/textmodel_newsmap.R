#' supervised multinomial classifier for multinomial text classification
#'
#' Train a model to predict geographical focus of texts
#' @param x dfm from which features will be extracted
#' @param y dfm in which features will be class labels
#' @param smooth smoothing parameter for word frequency
#' @param verbose if \code{TRUE}, show progress of training
#' @export
textmodel_newsmap <- function(x, y, smooth = 1, verbose = quanteda::quanteda_options('verbose')) {

    if (!is.dfm(x) || !is.dfm(y))
        stop('x and y have to be dfms')
    model <- matrix(rep(0, ncol(x) * ncol(y)), ncol = ncol(x), nrow = ncol(y),
                    dimnames = list(colnames(y), colnames(x)))
    if (verbose)
        cat("Training for class: ")
    for (key in sort(featnames(y))) {
        if (verbose)
            cat(key, " ", sep = "")
        temp <- quanteda::dfm_group(x, ifelse(as.vector(y[,key]) > 0, 'T', 'R'))
        missing <- setdiff(c('T', 'R'), rownames(temp))
        attr(temp, 'Dim')[1L] <- attr(temp, 'Dim')[1L] + length(missing)
        attr(temp, 'Dimnames')$docs <- c(attr(temp, 'Dimnames')$docs, missing)

        # words parameters
        temp <- temp + smooth
        sums <- Matrix::rowSums(temp)
        if (sums['T'] > 1) {
            temp2 <- (temp) / (sums) # conditional likelihood
            model[key,] <- as.vector(log(temp2['T',]) - log(temp2['R',])) # calculate likelihood-ratio
        } else {
            model[key,] <- NULL
        }
    }
    if (verbose)
        cat("\n")
    result <- list(model = model, data = x, feature = colnames(model))
    class(result) <- "textmodel_newsmap_fitted"
    return(result)
}

#' prediction method for textmodel_newsmap
#'
#' Predict document class using trained a Newsmap model
#' @param object a fitted Newsmap textmodel
#' @param newdata dfm on which prediction should be made
#' @param confidence.fit if \code{TRUE}, likelihood ratio score will be returned
#' @param rank rank of class to be predicted
#' @param type if \code{top}, return the most likely class specified by
#'   \code{rank}; otherswise return a matrix of lilelyhood ratio scores for all
#'   possible classes
#' @export
predict.textmodel_newsmap_fitted <- function(object, newdata = NULL, confidence.fit = FALSE, rank = 1L,
                                             type = c("top", "all")) {

    type <- match.arg(type)
    if (rank < 1L || !is.integer(rank)) {
        stop('rank must be positive integer')
    }
    if (is.null(newdata)) {
        data <- object$data
    } else {
        data <- newdata
    }
    model <- object$model
    data <- dfm_select(data, as.dfm(model))
    data <- quanteda::dfm_weight(data, 'relfreq')
    temp <- data %*% Matrix::t(as(model, 'denseMatrix'))

    if (type == 'top') {
        if (confidence.fit) {
            result <- list(class = apply(temp, 1, function(x) names(sort(x, decreasing = TRUE))[rank]),
                           confidence.fit = unname(apply(temp, 1, function(x) sort(x, decreasing = TRUE)[rank])))
            names(result$class) <- docnames(data)
        } else {
            result <- apply(temp, 1, function(x) names(sort(x, decreasing = TRUE))[rank])
            names(result) <- docnames(data)
        }
    } else {
        result <- temp[,!apply(temp, 2, function(x) all(x == 0))] # drop if all words are zero
        names(result) <- docnames(data)
    }

    return(result)

}

#' Evaluate classification accuracy in precision and recall
#' @param x vercor of predicted classes
#' @param y vector of true classes
#' @export
accuracy <- function(x, y) {

    temp <- data.frame(test = x, true = y)
    temp <- temp[!is.na(temp$true),] # remove unknown in true class

    classes <- unique(temp$true)
    result <- data.frame()
    for(class in classes){
        n <- sum(temp$true == class)
        tp <- sum(temp$true == class & temp$test == class)
        fp <- sum(temp$true != class & temp$test == class)
        fn <- sum(temp$true == class & temp$test != class)
        precision <- tp / (tp + fp)
        recall <- tp / (tp + fn)
        result <- rbind(result, data.frame(n, tp, fp, fn, precision, recall))
    }
    class(result) <- c('textmodel_newsmap_accuracy', class(result))
    return(result)
}

#' Summary method for fitted Newsmap model
#' @noRd textmodel_newsmap
#' @param object a fitted Newsmap textmodel
#' @param n number of classes, features and document names to be shown
#' @export
summary.textmodel_newsmap_fitted <- function(object, n = 10, ...) {
    result <- list(classes = head(rownames(object$model), n),
                   features = head(colnames(object$model), n),
                   documents = head(rownames(object$data), n))
    class(result) <- 'textmodel_newsmap_summary'
    return(result)
}

#' Summary method for fitted Newsmap model
#' @noRd textmodel_newsmap
#' @param object a fitted Newsmap textmodel
#' @export
print.textmodel_newsmap_summary <- function(object, ...) {
    cat('Classes:\n')
    cat('  ', paste0(object$classes , collapse = ', '), '... ', '\n')
    cat('Features:\n')
    cat('  ', paste0(object$features, collapse = ', '), '... ', '\n')
    cat('Documents:\n')
    cat('  ', paste0(object$documents, collapse = ', '), '... ', '\n')
}

#' Print micro and macro average measures of accuracy
#' @param accuracy output of accuracy()
#' @export
summary.textmodel_newsmap_accuracy <- function(mx) {

    #Micro-average of precision = (TP1+TP2)/(TP1+TP2+FP1+FP2)
    p <- sum(mx[,'tp'], na.rm=T) / sum(mx[,c('tp', 'fp')])
    #Micro-average of recall = (TP1+TP2)/(TP1+TP2+FN1+FN2)
    r <- sum(mx[,'tp'], na.rm=T) / sum(mx[,c('tp', 'fn')])
    #Macro-average precision = (P1+P2)/2
    P <- sum(mx[,'precision'], na.rm=T) / nrow(mx)
    #Macro-average recall = (R1+R2)/2
    R <- sum(mx[,'recall'], na.rm=T) / nrow(mx)

    result <- c(p = p, r = r, P = P, R = R)
    return(result)
}

