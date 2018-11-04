require(quanteda)

test_that("test English dictionary and prediction work correctly", {
    text_en <- c("This is an article about Ireland.")

    toks_en <- tokens(text_en)
    label_toks_en <- tokens_lookup(toks_en, data_dictionary_newsmap_en, levels = 3)
    label_dfm_en <- dfm(label_toks_en)

    feat_dfm_en <- dfm(toks_en, tolower = FALSE) %>%
        dfm_select('^[A-Z][A-Za-z1-2]+', selection = "keep",
                             valuetype = 'regex', case_insensitive = FALSE)
    expect_equal(
        predict(textmodel_newsmap(feat_dfm_en, label_dfm_en)),
        factor(c(text1 = "ie"), levels = colnames(label_dfm_en))
    )
})


test_that("test German dictionary and prediction work correctly", {
    text_de <- c("Ein Artikel über Irland.")

    toks_de <- tokens(text_de)
    label_toks_de <- tokens_lookup(toks_de, data_dictionary_newsmap_de, levels = 3)
    label_dfm_de <- dfm(label_toks_de)

    feat_dfm_de <- dfm(toks_de, tolower = FALSE) %>%
        dfm_select('^[A-Z][A-Za-z1-2]+', selection = "keep",
                             valuetype = 'regex', case_insensitive = FALSE)

    expect_equal(
        predict(textmodel_newsmap(feat_dfm_de, label_dfm_de)),
        factor(c(text1 = "ie"), levels = colnames(label_dfm_de))
    )
})

test_that("test Japanese dictionary and prediction work correctly", {
    text_ja <- c("アイルランドに関するテキスト.")

    toks_ja <- tokens(text_ja)
    label_toks_ja <- tokens_lookup(toks_ja, data_dictionary_newsmap_ja, levels = 3)
    label_dfm_ja <- dfm(label_toks_ja)

    feat_dfm_ja <- dfm(toks_ja, tolower = FALSE) %>%
        dfm_select('^[ぁ-ん]+$', selection = "remove", valuetype = 'regex')

    expect_equal(
        predict(textmodel_newsmap(feat_dfm_ja, label_dfm_ja)),
        factor(c(text1 = "ie"), levels = colnames(label_dfm_ja))
    )
})

test_that("test methods on textmodel_newsmap works correctly", {

    text <- c("Ireland is famous for Guinness.",
              "Guinness began retailing in India in 2007.",
              "Cork is an Irish coastal city.",
              "Titanic departed Cork Harbour in 1912.")

    toks <- tokens(text)
    label_toks <- tokens_lookup(toks, data_dictionary_newsmap_en, levels = 3)
    label_dfm <- dfm(label_toks)

    feat_dfm <- dfm(toks, tolower = FALSE) %>%
        dfm_select('^[A-Z][A-Za-z1-2]+', selection = "keep",
                             valuetype = 'regex', case_insensitive = FALSE)
    map <- textmodel_newsmap(feat_dfm, label_dfm)

    # class association is calculated correctly
    # note: both Guinness and Cork occur in IE only once
    expect_equivalent(map$model['ie', c('Ireland', 'Guinness')],
                      map$model['ie', c('Irish', 'Cork')] )
    expect_identical(map$feature, featnames(feat_dfm))

    # rank argument is working
    expect_equal(unname(predict(map)),
                 factor(c("ie", "in", "ie", "ie"), levels = colnames(label_dfm)))
    expect_equal(unname(predict(map, rank = 2)),
                 factor(c("in", "ie", "in", "in"), levels = colnames(label_dfm)))
    expect_error(predict(map, rank = 0))

    # different prediction outputs agree
    pred_top <- predict(map, confidence.fit = TRUE)
    pred_all <- predict(map, type = 'all')
    expect_equivalent(pred_top$confidence.fit, apply(pred_all, 1, max))
    expect_equivalent(pred_top$confidence.fit[1], pred_top$confidence.fit[3])

})

test_that("test raise error if dfm is empty", {
    expect_error(textmodel_newsmap(dfm_trim(dfm("a b c"), min_termfreq = 10), dfm("A")),
                 "x must have at least one non-zero feature")

    expect_error(textmodel_newsmap(dfm("a b c"), dfm_trim(dfm("A"), min_termfreq = 10)),
                 "y must have at least one non-zero feature")
})
