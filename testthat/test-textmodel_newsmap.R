test_that("test that English dictionary and prediction work correctly", {
    text_en <- c("This is an article about Ireland.")

    toks_en <- quanteda::tokens(text_en)
    label_toks_en <- quanteda::tokens_lookup(toks_en, data_dictionary_newsmap_en, levels = 3)
    label_dfm_en <- quanteda::dfm(label_toks_en)

    feat_dfm_en <- quanteda::dfm(toks_en, tolower = FALSE) %>%
        quanteda::dfm_select('^[A-Z][A-Za-z1-2]+', selection = "keep",
                             valuetype = 'regex', case_insensitive = FALSE)
    expect_equal(
        as.character(predict(textmodel_newsmap(feat_dfm_en, label_dfm_en))),
        "ie"
    )
})


test_that("test that German dictionary and prediction work correctly", {
    text_de <- c("Ein Artikel über Irland.")

    toks_de <- quanteda::tokens(text_de)
    label_toks_de <- quanteda::tokens_lookup(toks_de, data_dictionary_newsmap_de, levels = 3)
    label_dfm_de <- quanteda::dfm(label_toks_de)

    feat_dfm_de <- quanteda::dfm(toks_de, tolower = FALSE) %>%
        quanteda::dfm_select('^[A-Z][A-Za-z1-2]+', selection = "keep",
                             valuetype = 'regex', case_insensitive = FALSE)

    expect_equal(
        as.character(predict(textmodel_newsmap(feat_dfm_de, label_dfm_de))),
        "ie"
    )
})

test_that("test that Japanese dictionary and prediction work correctly", {
    text_ja <- c("アイルランドに関するテキスト.")

    toks_ja <- quanteda::tokens(text_ja)
    label_toks_ja <- quanteda::tokens_lookup(toks_ja, data_dictionary_newsmap_ja, levels = 3)
    label_dfm_ja <- quanteda::dfm(label_toks_ja)

    feat_dfm_ja <- quanteda::dfm(toks_ja, tolower = FALSE) %>%
        quanteda::dfm_select('^[A-Z][A-Za-z1-2]+', selection = "keep",
                             valuetype = 'regex', case_insensitive = FALSE)

    expect_equal(
        as.character(predict(textmodel_newsmap(feat_dfm_ja, label_dfm_ja))),
        "ie"
    )
})
