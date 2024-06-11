#' Semi-supervised Bayesian multinomial model for geographical document
#' classification
#'
#' Train a Newsmap model to predict geographical focus of documents with labels
#' given by a dictionary.
#' @param x a dfm or fcm created by [quanteda::dfm()]
#' @param y a dfm or a sparse matrix that record class membership of the
#'   documents. It can be created applying [quanteda::dfm_lookup()] to `x`.
#' @param label if "max", uses only labels for the maximum value in each row of
#'   `y`.
#' @param drop_label if `TRUE`, drops empty columns of `y` and ignore their
#'   labels.
#' @param smooth a value added to the frequency of words to smooth likelihood
#'   ratios.
#' @param boolean if `TRUE`, only consider presence or absence of features in
#'   each document to limit the impact of words repeated in few documents.
#' @param verbose if `TRUE`, shows progress of training.
#' @param entropy \[experimental\] the scheme to compute the entropy to
#'   regularize likelihood ratios. The entropy of features are computed over
#'   labels if `global` or over documents with the same labels if `local`. Local
#'   entropy is averaged if `average`. See the details.
#' @param ... additional arguments passed to internal functions.
#' @details Newsmap learns association between words and classes as likelihood
#'   ratios based on the features in `x` and the labels in `y`. The large
#'   likelihood ratios tend to concentrate to a small number of features but the
#'   entropy of their frequencies over labels or documents helps to disperse the
#'   distribution.
#'
#' @importFrom quanteda is.dfm as.dfm dfm_trim nfeat
#' @references Kohei Watanabe. 2018. "[Newsmap: semi-supervised approach to
#'   geographical news
#'   classification.](https://www.tandfonline.com/eprint/dDeyUTBrhxBSSkHPn5uB/full)"
#'    *Digital Journalism* 6(3): 294-309.
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
#' @export
textmodel_newsmap <- function(x, y, label = c("all", "max"), smooth = 1.0,
                              boolean = FALSE, drop_label = TRUE,
                              verbose = quanteda_options('verbose'),
                              entropy = c("none", "global", "local", "average"), ...) {
    UseMethod("textmodel_newsmap")
}

#' @noRd
#' @export
#' @importFrom quanteda check_double check_logical
textmodel_newsmap.dfm <- function(x, y, label = c("all", "max"), smooth = 1.0,
                                  boolean = FALSE, drop_label = TRUE,
                                  verbose = quanteda_options('verbose'),
                                  entropy = c("none", "global", "local", "average"), ...) {

    unused_dots(...)
    entropy <- match.arg(entropy)
    label <- match.arg(label)
    smooth <- check_double(smooth, min = 0)
    boolean <- check_logical(boolean)
    drop_label <- check_logical(drop_label)

    if (label == "max") {
        y <- as(as(as(y, "dMatrix"), "generalMatrix"), "TsparseMatrix")
        s <- sapply(split(y@x, y@i + 1L), max)
        y@x[y@x < s[y@i + 1L]] <- 0L
    }

    w <- dfm_trim(x, min_termfreq = 1)
    if (boolean)
        w <- dfm_weight(w, "boolean")
    y <- as.dfm(y)
    if (drop_label)
        y <- dfm_trim(y, min_termfreq = 1)

    if (!nfeat(w))
        stop("x must have at least one non-zero feature")
    if (!nfeat(y))
        stop("y must have at least one non-zero feature")

    model <- matrix(rep(0, ncol(w) * ncol(y)), ncol = ncol(w), nrow = ncol(y),
                    dimnames = list(colnames(y), colnames(w)))

    if (verbose)
        cat("Fitting textmodel_newsmap...\n")

    if (entropy == "global") {
        e <- get_entropy(w, nrow(w)) # e = 1.0 for uniform distribution
        weight <- matrix(rep(e, each = ncol(y)), ncol = ncol(w), nrow = ncol(y),
                         dimnames = list(colnames(y), colnames(w)))
    } else {
        weight <- matrix(rep(1, ncol(w) * ncol(y)), ncol = ncol(w), nrow = ncol(y),
                         dimnames = list(colnames(y), colnames(w)))
    }

    m <- colSums(w)
    for (key in sort(featnames(y))) {
        if (verbose) cat(sprintf('  label = "%s"\n', key))
        z <- w[as.logical(y[,key] > 0),]
        s <- colSums(z)
        v0 <- m - s + smooth
        v1 <- s + smooth
        model[key,] <- log(v1 / sum(v1)) - log(v0 / sum(v0)) # log-likelihood ratio

        if (entropy %in% c("local", "average")) {
            if (nrow(z) > 1) {
                weight[key,] <- get_entropy(z, nrow(z)) # e = 1.0 for uniform distribution
            } else {
                if (entropy == "local") {
                    weight[key,] <- 0
                } else {
                    weight[key,] <- NA
                }
            }
        }
    }

    if (entropy == "average") {
        e <- colMeans(weight, na.rm = TRUE)
        weight <- matrix(rep(e, each = ncol(y)), ncol = ncol(w), nrow = ncol(y),
                         dimnames = list(colnames(y), colnames(w)))
    }

    result <- list(model = model,
                   entropy = entropy,
                   data = x,
                   weight = NULL,
                   feature = colnames(model),
                   concatenator = meta(x, field = "concatenator", type = "object"),
                   call = match.call(sys.function(-1), call = sys.call(-1)),
                   version = utils::packageVersion("newsmap"))
    if (entropy != "none")
        result$weight <- weight
    class(result) <- "textmodel_newsmap"
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

#' Extract coefficients for features
#' @param object a Newsmap model fitted by [textmodel_newsmap()].
#' @param n the number of coefficients to extract.
#' @param select returns the coefficients for the selected class; specify by the
#'   names of rows in `object$model`.
#' @param ... not used.
#' @method coef textmodel_newsmap
#' @import Matrix
#' @importFrom stats coef
#' @export
coef.textmodel_newsmap <- function(object, n = 10, select = NULL, ...) {

    n <- check_integer(n, min = 0)
    select <- check_character(select, min_len = 1, max_len = nrow(object$model),
                              strict = TRUE, allow_null = TRUE)

    if (is.null(select)) {
        j <- rep(TRUE, nrow(object$model))
    } else {
        if (any(!select %in% rownames(object$model)))
            stop("Selected class must be in the model", call. = FALSE)
        j <- rownames(object$model) %in% select
    }

    if (is.null(object$weight)){
        model <- object$model
    } else {
        model <- object$model * object$weight
    }

    model <- as(as(as(model, "dMatrix"), "generalMatrix"), "TsparseMatrix")
    model <- model[j,, drop = FALSE]
    temp <- model@x
    names(temp) <- colnames(object$model)[model@j + 1L]
    result <- split(temp, factor(model@i + 1L, levels = seq_len(nrow(model)),
                                 labels = rownames(model)))
    result <- lapply(result, function(x) head(sort(x, decreasing = TRUE), n))
    return(result)
}

#' @rdname coef.textmodel_newsmap
#' @method coefficients textmodel_newsmap
#' @importFrom stats coefficients
#' @export
coefficients.textmodel_newsmap <- function(object, n = 10, select = NULL, ...) {
    UseMethod("coef")
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

    label <- sort(unique(temp$true))
    result <- data.frame()
    for(l in label){
        tp <- sum(temp$true == l & temp$test == l, na.rm = TRUE)
        fp <- sum(temp$true != l & temp$test == l, na.rm = TRUE)
        tn <- sum(temp$true != l & temp$test != l, na.rm = TRUE)
        fn <- sum(temp$true == l & temp$test != l, na.rm = TRUE)
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
    x <- as(as(as(x, "dMatrix"), "generalMatrix"), "TsparseMatrix")
    result <- unlist(lapply(split(x@x, factor(x@i + 1L, levels = seq_len(nrow(x)))),
                       function(y) sum(y * log(y, base)) * -1), use.names = FALSE)
    names(result) <- rownames(x)
    return(result)
}
