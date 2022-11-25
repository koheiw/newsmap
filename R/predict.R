
#' Prediction method for textmodel_newsmap
#'
#' Predict document class using trained a Newsmap model
#' @param object a fitted Newsmap textmodel.
#' @param newdata dfm on which prediction should be made.
#' @param confidence if `TRUE`, it returns likelihood ratio score.
#' @param rank rank of the class to be predicted. Only used when `type = "top"`.
#' @param type if `top`, returns the most likely class specified by `rank`;
#'   otherwise return a matrix of likelihood ratio scores for all possible
#'   classes.
#' @param rescale if `TRUE`, likelihood ratio scores are normalized using [scale()]. This affects
#'   both types of results.
#' @param min_conf return `NA` when confidence is lower than this value.
#' @param min_n set the minimum number of polarity words in documents.
#' @param ... not used.
#' @method predict textmodel_newsmap
#' @export
#' @importFrom methods as
#' @importFrom quanteda dfm_match dfm_weight docnames featnames quanteda_options
predict.textmodel_newsmap <- function(object, newdata = NULL, confidence = FALSE, rank = 1L,
                                      type = c("top", "all"), rescale = FALSE,
                                      min_conf = -Inf, min_n = 0L, ...) {

    args <- list(...)
    if ("confidence.fit" %in% names(args)) {
        .Deprecated(msg = "'confidence.fit' is deprecated; use 'confidence'\n")
        confidence <- args$confidence.fit
    }


    min_conf <- check_double(min_conf)
    min_n <- check_integer(min_n, min = 0)
    type <- match.arg(type)

    if (is.null(newdata)) {
        data <- object$data
    } else {
        data <- newdata
    }
    if (is.null(object$weight)){
        model <- object$model
    } else {
        model <- object$model * object$weight
    }
    data <- dfm_match(data, colnames(model))
    len <- unname(rowSums(data))
    n <- ifelse(len == 0, 0, pmax(len, min_n))
    temp <- Matrix::tcrossprod(data, model) / n
    is_empty <- len == 0

    if (rescale)
        temp <- as(scale(temp), 'denseMatrix')

    rank <- check_integer(rank, min = 1, max = ncol(temp))
    if (type == 'top') {
        if (confidence) {
            if (ncol(temp)) {
                result <- list(class = get_nth(temp, rank, "class"),
                               confidence.fit = unname(get_nth(temp, rank, "conf")))
            } else {
                result$class <- rep(NA, nrow(temp))
            }
            result$class[result$confidence.fit < min_conf] <- NA
            result$class[is_empty] <- NA
            result$confidence.fit[is_empty] <- NA
            names(result$class) <- docnames(data)
            result$class <- factor(result$class, levels = rownames(model))
        } else {
            if (ncol(temp)) {
                result <- get_nth(temp, rank, "class")
                if (min_conf != -Inf)
                    result[get_nth(temp, rank, "conf") <- min_conf] <- NA
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
