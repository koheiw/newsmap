
#' @export
#' @method print textmodel_newsmap
print.textmodel_newsmap <- function(x, ...) {
    cat("\nCall:\n")
    print(x$call)
    cat("\n")
    if (!is.null(x$model)) {
        cat("Labels:\n", sep = "")
        print(rownames(x$model))
    }
}
