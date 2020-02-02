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
#' @param verbose if \code{TRUE}, show progress of training
#' @import quanteda
#' @references Kohei Watanabe. 2018.
#'   "\href{http://www.tandfonline.com/eprint/dDeyUTBrhxBSSkHPn5uB/full}{Newsmap:
#'    semi-supervised approach to geographical news classification.}"
#'   \emph{Digital Journalism} 6(3): 294-309.
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
textmodel_newsmap <- function(x, y, smooth = 1, verbose = quanteda_options('verbose')) {

    if (!is.dfm(x) || !is.dfm(y))
        stop('x and y have to be dfm')

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
    m <- colSums(x)
    for (key in sort(featnames(y))) {
        if (verbose)
            cat(key, " ", sep = "")
        s <- colSums(x[as.logical(y[,key] > 0),])
        v1 <- s + smooth
        v0 <- m - s + smooth
        model[key,] <- log(v1 / sum(v1)) - log(v0 / sum(v0)) # log-likelihood ratio
    }
    if (verbose)
        cat("\n")
    result <- list(model = model, data = x, feature = colnames(model))
    class(result) <- "textmodel_newsmap"
    return(result)
}

#' Prediction method for textmodel_newsmap
#'
#' Predict document class using trained a Newsmap model
#' @param object a fitted Newsmap textmodel
#' @param newdata dfm on which prediction should be made
#' @param confidence.fit if \code{TRUE}, likelihood ratio score will be returned
#' @param rank rank of class to be predicted. Only used when \code{type = "top"}.
#' @param type if \code{top}, return the most likely class specified by
#'   \code{rank}; otherswise return a matrix of lilelyhood ratio scores for all
#'   possible classes
#' @param ... not used.
#' @method predict textmodel_newsmap
#' @export
#' @import quanteda methods
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
                result <- list(class = apply(temp, 1, function(x) names(get_nth(x, rank))),
                               confidence.fit = unname(apply(temp, 1, function(x) get_nth(x, rank))))
            } else {
                result$class <- rep(NA, nrow(temp))
            }
            result$class[is_empty] <- NA
            result$confidence.fit[is_empty] <- NA
            names(result$class) <- docnames(data)
        } else {
            if (ncol(temp)) {
                result <- apply(temp, 1, function(x) names(get_nth(x, rank)))
            } else {
                result <- rep(NA, nrow(temp))
            }
            result[is_empty] <- NA
            names(result) <- docnames(data)
        }
    } else {
        result <- temp
        result[is_empty,] <- NA
        rownames(result) <- docnames(data)
    }
    return(result)
}

get_nth <- function(x, rank) {
    sort(x, decreasing = TRUE)[rank]
}

#' @noRd
#' @method summary textmodel_newsmap
#' @export
summary.textmodel_newsmap <- function(object, n = 10, ...) {
    result <- list(classes = utils::head(rownames(object$model), n),
                   features = utils::head(colnames(object$model), n),
                   documents = utils::head(rownames(object$data), n))
    class(result) <- 'textmodel_newsmap_summary'
    return(result)
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
    result <- split(temp, model@i)
    names(result) <- rownames(object$model)
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
#' @param x vercor of predicted classes
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

#' Calcualte micro and macro average measures of accuracy
#'
#' This function calculates micro-averave precision (p) and recall (r) and
#' macro-average precision (P) and recall (R) based on a confusion matrix from
#' \code{accuracy()}.
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
#' AFE computes randomness of occurences features in labeled documents.
#' @param x a dfm for features
#' @param y a dfm for labels
#' @param smooth a numeric value for smoothing to include all the features
#' @export
afe <- function(x, y, smooth = 1) {
    if (!is.dfm(x) || !is.dfm(y))
        stop('x and y have to be dfm')
    e <- textstat_entropy(group_topics(x, y) + smooth,
                          margin = "features")
    if (is.data.frame(e))
        e <- e$entropy
    return(mean(e))
}

group_topics <- function(x, y) {
    result <- matrix(NA, nrow = nfeat(y), ncol = nfeat(x),
                     dimnames = list(featnames(y), featnames(x)))
    for (i in seq_len(nfeat(y))) {
        result[i,] <- colSums(dfm_subset(x, rowSums(y[,i]) > 0))
    }
    return(as.dfm(result))
}


