#' Seed geographical dictionary in English
#'
#' @name data_dictionary_newsmap_en
#' @docType data
#' @author Kohei Watanabe \email{watanabe.kohei@gmail.com}
#' @keywords data
NULL

#' Seed geographical dictionary in Japanese
#'
#' @name data_dictionary_newsmap_ja
#' @docType data
#' @author Kohei Watanabe \email{watanabe.kohei@gmail.com}
#' @keywords data
NULL

#' Seed geographical dictionary in German
#'
#' @name data_dictionary_newsmap_de
#' @docType data
#' @author Stefan Müller \email{mullers@tcd.ie}
#' @keywords data
NULL

#' @noRd
#' @keywords internal
#' @examples
#' data_dictionary_newsmap_en <- import_dictionary('data/english.yml')
#' save(data_dictionary_newsmap_en, file = 'dict/data_dictionary_newsmap_en.RData')
#' data_dictionary_newsmap_ja <- import_dictionary('data/japanese.yml')
#' save(data_dictionary_newsmap_ja, file = 'dict/data_dictionary_newsmap_ja.RData')
#' data_dictionary_newsmap_de <- import_dictionary('data/german.yml')
#' save(data_dictionary_newsmap_de, file = 'dict/data_dictionary_newsmap_de.RData')
import_dictionary <- function(file) {
    quanteda::dictionary(file = file)
}
