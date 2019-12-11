context("test data")

require(quanteda)
require(stringi)
require(testthat)

check_character <- function(x) {
    x <- unlist(x)
    ## \\u30fb katakana middle dot
    ## \\p{M} combining character
    expect_true(all(stri_detect_regex(x, "^[\\p{L}\\p{M}'\"*?\\-\\u30fb ]+$")))
}

test_that("test that yaml do not contain illegal letters", {
    check_character(data_dictionary_newsmap_en)
    check_character(data_dictionary_newsmap_de)
    check_character(data_dictionary_newsmap_fr)
    check_character(data_dictionary_newsmap_es)
    check_character(data_dictionary_newsmap_ja)
    check_character(data_dictionary_newsmap_ru)
    check_character(data_dictionary_newsmap_he)
    check_character(data_dictionary_newsmap_ar)
    check_character(data_dictionary_newsmap_zh_cn)
    check_character(data_dictionary_newsmap_zh_tw)
})

test_that("test that data file is created correctly", {

    expect_equal(length(names(data_dictionary_newsmap_en)), 5)
    expect_equal(length(names(data_dictionary_newsmap_de)), 5)
    expect_equal(length(names(data_dictionary_newsmap_fr)), 5)
    expect_equal(length(names(data_dictionary_newsmap_es)), 5)
    expect_equal(length(names(data_dictionary_newsmap_ja)), 5)
    expect_equal(length(names(data_dictionary_newsmap_ru)), 5)
    expect_equal(length(names(data_dictionary_newsmap_it)), 5)
    expect_equal(length(names(data_dictionary_newsmap_he)), 5)
    expect_equal(length(names(data_dictionary_newsmap_ar)), 5)
    expect_equal(length(names(data_dictionary_newsmap_zh_cn)), 5)
    expect_equal(length(names(data_dictionary_newsmap_zh_tw)), 5)
})


test_that("test that dictionaries have the same countries", {

    en <- names(quanteda:::flatten_dictionary(data_dictionary_newsmap_en))
    de <- names(quanteda:::flatten_dictionary(data_dictionary_newsmap_de))
    fr <- names(quanteda:::flatten_dictionary(data_dictionary_newsmap_fr))
    es <- names(quanteda:::flatten_dictionary(data_dictionary_newsmap_es))
    ja <- names(quanteda:::flatten_dictionary(data_dictionary_newsmap_ja))
    ru <- names(quanteda:::flatten_dictionary(data_dictionary_newsmap_ru))
    it <- names(quanteda:::flatten_dictionary(data_dictionary_newsmap_it))
    he <- names(quanteda:::flatten_dictionary(data_dictionary_newsmap_he))
    ar <- names(quanteda:::flatten_dictionary(data_dictionary_newsmap_ar))
    zh_cn <- names(quanteda:::flatten_dictionary(data_dictionary_newsmap_zh_cn))
    zh_tw <- names(quanteda:::flatten_dictionary(data_dictionary_newsmap_zh_tw))

    expect_true(identical(en, de))
    expect_true(identical(en, fr))
    expect_true(identical(en, es))
    expect_true(identical(en, ja))
    expect_true(identical(en, ru))
    expect_true(identical(en, it))
    expect_true(identical(en, he))
    expect_true(identical(en, ar))
    expect_true(identical(en, zh_cn))
    expect_true(identical(en, zh_tw))
})
