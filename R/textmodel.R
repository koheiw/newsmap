#' Semi-supervised Bayesian multinomial model for geographical document
#' classification
#'
#' Train a Newsmap model to predict geographical focus of documents using a
#' pre-defined seed dictionary. Currently seed dictionaries are available in
#' English (en), German (de), Spanish (es), Japanese (ja), Russian (ru) and
#' Chinese (zh).
#' @param x dfm from which features will be extracted
#' @param y dfm in which features will be class labels
#' @param smooth smoothing parameter for word frequency
#' @param verbose if `TRUE`, show progress of training
#' @importFrom quanteda is.dfm dfm_trim nfeat
#' @references Kohei Watanabe. 2018.
#'   "[Newsmap:
#'    semi-supervised approach to geographical news classification.](https://www.tandfonline.com/eprint/dDeyUTBrhxBSSkHPn5uB/full)"
#'   *Digital Journalism* 6(3): 294-309.
#' @export
#' @examples
#' require(quanteda)
#' text_en <- c(text1 = "This is an article about Ireland.",
#'              text2 = "The South Korean prime minister was re-elected.")
#'
#' toks_en <- tokens(text_en)
#' label_toks_en <- tokens_lookup(toks_en, data_dictionary_newsmap_en, levels = 3)
#' label_dfm_en <- dfm(label_toks_en)
#'
#' feat_dfm_en <- dfm(toks_en, tolower = FALSE)
#'
#' model_en <- textmodel_newsmap(feat_dfm_en, label_dfm_en)
#' predict(model_en)
#'
#'
textmodel_newsmap <- function(x, y, measure = c("likelihood", "entropy"),
                              weight = c("none", "entropy", "entropy2"), smooth = 1.0,
                              verbose = quanteda_options('verbose')) {

    if (!is.dfm(x) || !is.dfm(y))
        stop('x and y have to be dfm')

    measure <- match.arg(measure)
    weight <- match.arg(weight)

    #if (smooth >= 1.0)
    #    warning("The value of smooth should be fractional after v0.8.0. See the manual for the detail.")

    x <- dfm_trim(x, min_termfreq = 1)
    y <- dfm_trim(y, min_termfreq = 1)

    if (!nfeat(x))
        stop("x must have at least one non-zero feature")
    if (!nfeat(y))
        stop("y must have at least one non-zero feature")

    model <- matrix(rep(0, ncol(x) * ncol(y)), ncol = ncol(x), nrow = ncol(y),
                    dimnames = list(colnames(y), colnames(x)))
    if (verbose)
        cat("Training for class: ")

    if (measure == "likelihood") {
        m <- colSums(x)
        if (weight == "entropy2")
            e <- get_entropy(x, nrow(x)) # e = 1.0 for uniform distribution
        for (key in sort(featnames(y))) {
            if (verbose)
                cat(key, " ", sep = "")
            z <- x[as.logical(y[,key] > 0),]
            s <- colSums(z)
            v0 <- m - s + smooth
            v1 <- s + smooth
            lr <- log(v1 / sum(v1)) - log(v0 / sum(v0)) # log-likelihood ratio
            if (weight == "entropy") {
                e <- get_entropy(z, nrow(z)) # e = 1.0 for uniform distribution
            } else if (weight == "none") {
                e <- 1.0
            }
            model[key,] <- lr * e
        }
    } else if (measure == "entropy") { # TODO: change to conditional entropy
        e0 <- get_entropy(group_topics(x, y), ncol(y))
        for (key in sort(featnames(y))) {
            if (verbose)
                cat(key, " ", sep = "")
            z <- x[as.logical(y[,key] > 0),]
            e1 <- get_entropy(z, nrow(z))
            #model[key,] <- log(e1 + smooth) - log(e0 + smooth) # log-entropy ratio
            model[key,] <- log(e1 + smooth) - log(e0 + smooth) # log-entropy ratio
        }
    }

    if (verbose)
        cat("\n")
    result <- list(model = model,
                   measure = measure,
                   data = x,
                   feature = colnames(model),
                   call = match.call())
    class(result) <- "textmodel_newsmap"
    return(result)
}

#' Prediction method for textmodel_newsmap
#'
#' Predict document class using trained a Newsmap model
#' @param object a fitted Newsmap textmodel
#' @param newdata dfm on which prediction should be made
#' @param confidence.fit if `TRUE`, likelihood ratio score will be returned
#' @param rank rank of class to be predicted. Only used when `type = "top"`.
#' @param type if `top`, returns the most likely class specified by
#'   `rank`; otherwise return a matrix of likelihood ratio scores for all
#'   possible classes
#' @param ... not used.
#' @method predict textmodel_newsmap
#' @export
#' @importFrom methods as
#' @importFrom quanteda dfm_match dfm_weight docnames featnames quanteda_options
predict.textmodel_newsmap <- function(object, newdata = NULL, confidence.fit = FALSE, rank = 1L,
                                      type = c("top", "all"), ...) {

    type <- match.arg(type)
    if (rank < 1 || !is.numeric(rank)) {
        stop('rank must be positive integer')
    }
    if (is.null(newdata)) {
        data <- object$data
    } else {
        data <- newdata
    }
    model <- object$model
    data <- dfm_match(data, colnames(model))
    data <- dfm_weight(data, 'prop')
    temp <- data %*% Matrix::t(as(model, 'denseMatrix'))
    is_empty <- rowSums(data) == 0

    if (type == 'top') {
        if (confidence.fit) {
            if (ncol(temp)) {
                result <- list(class = get_nth(temp, rank, "class"),
                               confidence.fit = unname(get_nth(temp, rank, "conf")))
            } else {
                result$class <- rep(NA, nrow(temp))
            }
            result$class[is_empty] <- NA
            result$confidence.fit[is_empty] <- NA
            names(result$class) <- docnames(data)
            result$class <- factor(result$class, levels = rownames(model))
        } else {
            if (ncol(temp)) {
                result <- get_nth(temp, rank, "class")
            } else {
                result <- rep(NA, nrow(temp))
            }
            result[is_empty] <- NA
            names(result) <- docnames(data)
            result <- factor(result, levels = rownames(model))
        }
    } else {
        result <- temp
        result[is_empty,] <- NA
        rownames(result) <- docnames(data)
    }
    return(result)
}

get_nth <- function(x, rank, type = c("class", "conf")) {

    type <- match.arg(type)
    for (i in seq_len(rank - 1)) {
        x <- replace(x, cbind(seq_len(nrow(x)), max.col(x)), -Inf)
    }
    if (type == "conf") {
        result <- x[cbind(seq_len(nrow(x)), max.col(x))]
    } else {
        result <- colnames(x)[max.col(x)]
    }
    names(result) <- rownames(x)
    return(result)
}

#' @noRd
#' @method summary textmodel_newsmap
#' @export
summary.textmodel_newsmap <- function(object, n = 10, ...) {
    result <- list(
        "call" = object$call,
        "labels" = rownames(object$model)
    )
    if (!is.null(object$data))
        result$data.dimension <- dim(object$data)
    as.summary.textmodel(result)
}

#' @noRd
#' @method coef textmodel_newsmap
#' @import Matrix
#' @importFrom stats coef
#' @export
coef.textmodel_newsmap <- function(object, n = 10, ...) {
    model <- as(object$model, "dgTMatrix")
    temp <- model@x
    names(temp) <- colnames(object$model)[model@j + 1]
    result <- split(temp, rownames(model)[model@i + 1])
    result <- lapply(result, function(x) head(sort(x, decreasing = TRUE), n))
    return(result)
}

#' @noRd
#' @method coefficients textmodel_newsmap
#' @importFrom stats coefficients
#' @export
coefficients.textmodel_newsmap <- function(object, n = 10, ...) {
    UseMethod("coef")
}

#' Print method for a fitted Newsmap model
#' @param x a fitted Newsmap textmodel
#' @param ... not used.
#' @method print textmodel_newsmap_summary
#' @export
print.textmodel_newsmap_summary <- function(x, ...) {
    cat('Classes:\n')
    cat('  ', paste0(x$classes , collapse = ', '), '... ', '\n')
    cat('Features:\n')
    cat('  ', paste0(x$features, collapse = ', '), '... ', '\n')
    cat('Documents:\n')
    cat('  ', paste0(x$documents, collapse = ', '), '... ', '\n')
}

#' Evaluate classification accuracy in precision and recall
#'
# Retuns a confusion matrix that contains number true positive (tp), fales
# positive (fp), true negative (tn) and false negative (fn) cases for each
# predicted class. It also calculates precision, recall and F1 score based on
# these counts.
#' @param x vector of predicted classes
#' @param y vector of true classes
#' @export
#' @examples
#' class_pred <- c('US', 'GB', 'US', 'CN', 'JP', 'FR', 'CN') # prediction
#' class_true <- c('US', 'FR', 'US', 'CN', 'KP', 'EG', 'US') # true class
#' acc <- accuracy(class_pred, class_true)
#' print(acc)
#' summary(acc)
accuracy <- function(x, y) {

    temp <- data.frame(test = x, true = y)
    temp <- temp[!is.na(temp$true),,drop = FALSE] # remove unknown in true class

    label <- unique(temp$true)
    result <- data.frame()
    for(l in label){
        tp <- sum(temp$true == l & temp$test == l)
        fp <- sum(temp$true != l & temp$test == l)
        tn <- sum(temp$true != l & temp$test != l)
        fn <- sum(temp$true == l & temp$test != l)
        precision <- tp / (tp + fp)
        recall <- tp / (tp + fn)
        f1 <- (2 * precision * recall) / (precision + recall)
        result <- rbind(result, data.frame(tp, fp, tn, fn, precision, recall, f1))
    }
    class(result) <- c('textmodel_newsmap_accuracy', class(result))
    rownames(result) <- label
    return(result)
}

#' Calculate micro and macro average measures of accuracy
#'
#' This function calculates micro-average precision (p) and recall (r) and
#' macro-average precision (P) and recall (R) based on a confusion matrix from
#' `accuracy()`.
#' @param object output of accuracy()
#' @param ... not used.
#' @method summary textmodel_newsmap_accuracy
#' @export
summary.textmodel_newsmap_accuracy <- function(object, ...) {

    #Micro-average of precision = (TP1+TP2)/(TP1+TP2+FP1+FP2)
    p <- sum(object[,'tp'], na.rm = TRUE) / sum(object[,c('tp', 'fp')])
    #Micro-average of recall = (TP1+TP2)/(TP1+TP2+FN1+FN2)
    r <- sum(object[,'tp'], na.rm = TRUE) / sum(object[,c('tp', 'fn')])
    #Macro-average precision = (P1+P2)/2
    P <- sum(object[,'precision'], na.rm = TRUE) / nrow(object)
    #Macro-average recall = (R1+R2)/2
    R <- sum(object[,'recall'], na.rm = TRUE) / nrow(object)

    result <- c(p = p, r = r, P = P, R = R)
    return(result)
}

#' Compute average feature entropy (AFE)
#'
#' AFE computes randomness of occurrences features in labelled documents.
#' @param x a dfm for features
#' @param y a dfm for labels
#' @param smooth a numeric value for smoothing to include all the features
#' @importFrom quanteda.textstats textstat_entropy
#' @importFrom quanteda is.dfm nfeat featnames colSums rowSums dfm_subset as.dfm
#' @export
afe <- function(x, y, smooth = 1) {
    if (!is.dfm(x) || !is.dfm(y))
        stop('x and y have to be dfm')
    e <- get_entropy(group_topics(x, y) + smooth)
    if (is.data.frame(e))
        e <- e$entropy
    return(mean(e))
}

group_topics <- function(x, y) {
    result <- matrix(NA, nrow = nfeat(y), ncol = nfeat(x),
                     dimnames = list(featnames(y), featnames(x)))
    for (i in seq_len(nfeat(y))) {
        result[i, ] <- colSums(dfm_subset(x, rowSums(y[ ,i]) > 0))
    }
    return(as.dfm(result))
}

get_entropy <- function(x, base = 2) {

    x <- t(x)
    x <- dfm_weight(x, "prop")
    x <- as(x, "dgTMatrix")
    result <- unlist(lapply(split(x@x, factor(x@i + 1L, levels = seq_len(nrow(x)))),
                       function(y) sum(y * log(y, base)) * -1), use.names = FALSE)
    names(result) <- rownames(x)
    return(result)
}
