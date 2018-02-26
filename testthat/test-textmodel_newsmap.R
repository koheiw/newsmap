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

test_that("test that methods on textmodel_newsmap works correctly", {

    text <- c("Ireland is famous for Guinness.",
              "Guinness began retailing in India in 2007.",
              "Cork is an Irish coastal city.",
              "Titanic departed from Cork Harbour in 1912.")

    toks <- quanteda::tokens(text)
    label_toks <- quanteda::tokens_lookup(toks, data_dictionary_newsmap_en, levels = 3)
    label_dfm <- quanteda::dfm(label_toks)

    feat_dfm <- quanteda::dfm(toks, tolower = FALSE) %>%
        quanteda::dfm_select('^[A-Z][A-Za-z1-2]+', selection = "keep",
                             valuetype = 'regex', case_insensitive = FALSE)
    map <- textmodel_newsmap(feat_dfm, label_dfm)

    # class association is calculated correctly
    # note: both Guinness and Cork occur in IE only once
    expect_equivalent(map$model['ie', c('Ireland', 'Guinness')],
                      map$model['ie', c('Irish', 'Cork')] )
    expect_identical(map$feature, featnames(feat_dfm))

    # rank argument is working
    expect_equal(unname(predict(map)), c("ie", "in", "ie", "ie"))
    expect_equal(unname(predict(map, rank = 2)), c("in", "ie", "in", "in"))
    expect_error(predict(map, rank = 0))

    # different prediction outputs agree
    pred_top <- predict(map, confidence.fit = TRUE)
    pred_all <- predict(map, type = 'all')
    expect_equivalent(pred_top$confidence.fit, apply(pred_all, 1, max))
    expect_equivalent(pred_top$confidence.fit[1], pred_top$confidence.fit[3])

})
