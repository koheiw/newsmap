#' seed geographical dictionary in English
#'
#' @name data_dictionary_newsmap_en
#' @docType data
#' @author Kohei Watanabe \email{watana.kohei@gmail.com}
#' @keywords data
NULL

#' seed geographical dictionary in Japanese
#'
#' @name data_dictionary_newsmap_ja
#' @docType data
#' @author Kohei Watanabe \email{watana.kohei@gmail.com}
#' @keywords data
NULL

#' @noRd
#' @keywords internal
#' @examples
#' data_dictionary_newsmap_en <- Newsmap:::import_dictionary('data/english.yml')
#' save(data_dictionary_newsmap_en, file = 'data/data_dictionary_newsmap_en.RData')
#' data_dictionary_newsmap_ja <- Newsmap:::import_dictionary('data/japanese.yml')
#' save(data_dictionary_newsmap_ja, file = 'data/data_dictionary_newsmap_ja.RData')
import_dictionary <- function(file) {
    quanteda::dictionary(file = file)
}
