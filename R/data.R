#' Seed geographical dictionary in English
#'
#' @name data_dictionary_newsmap_en
#' @docType data
#' @author Kohei Watanabe \email{watana.kohei@gmail.com}
#' @keywords data
NULL

#' Seed geographical dictionary in Japanese
#'
#' @name data_dictionary_newsmap_ja
#' @docType data
#' @author Kohei Watanabe \email{watana.kohei@gmail.com}
#' @keywords data
NULL

#' Seed geographical dictionary in German
#'
#' @name data_dictionary_newsmap_de
#' @docType data
#' @author Stefan Müller \email{mullers@tcd.ie}
#' @keywords data
NULL

#' Seed geographical dictionary in Russian
#'
#' @name data_dictionary_newsmap_ru
#' @docType data
#' @author Yulia Netesova \email{julianetesova@gmail.com}
#' @keywords data
NULL

#' @noRd
#' @keywords internal
#' @examples
#' data_dictionary_newsmap_en <- newsmap:::import_dictionary('data/english.yml')
#' save(data_dictionary_newsmap_en, file = 'data/data_dictionary_newsmap_en.RData')
#' data_dictionary_newsmap_ja <- newsmap:::import_dictionary('data/japanese.yml')
#' save(data_dictionary_newsmap_ja, file = 'data/data_dictionary_newsmap_ja.RData')
#' data_dictionary_newsmap_de <- newsmap:::import_dictionary('data/german.yml')
#' save(data_dictionary_newsmap_de, file = 'data/data_dictionary_newsmap_de.RData')
#' data_dictionary_newsmap_ru <- newsmap:::import_dictionary('data/russian.yml')
#' save(data_dictionary_newsmap_ru, file = 'data/data_dictionary_newsmap_ru.RData')
import_dictionary <- function(file) {
    quanteda::dictionary(file = file)
}
