
#' @export
#' @method print textmodel_newsmap
print.textmodel_newsmap <- function(x, ...) {
    cat("\nCall:\n")
    print(x$call)
    cat("\n")
}
