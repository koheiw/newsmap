require(quanteda)

test_that("English dictionary and prediction work correctly", {

    txt_en <- c("This is an article about Ireland.")
    toks_en <- tokens(txt_en)
    label_toks_en <- tokens_lookup(toks_en, data_dictionary_newsmap_en, levels = 3)
    label_dfm_en <- dfm(label_toks_en)

    feat_dfm_en <- dfm(toks_en, tolower = FALSE) %>%
        dfm_select('^[A-Z][A-Za-z1-2]+', selection = "keep",
                             valuetype = 'regex', case_insensitive = FALSE)
    expect_equal(
        predict(textmodel_newsmap(feat_dfm_en, label_dfm_en)),
        factor(c(text1 = "ie"), levels = "ie")
    )
})


test_that("German dictionary and prediction work correctly", {

    txt_de <- c("Ein Artikel über Irland.")
    toks_de <- tokens(txt_de)
    label_toks_de <- tokens_lookup(toks_de, data_dictionary_newsmap_de, levels = 3)
    label_dfm_de <- dfm(label_toks_de)

    feat_dfm_de <- dfm(toks_de, tolower = FALSE) %>%
        dfm_select('^[A-Z][A-Za-z1-2]+', selection = "keep",
                             valuetype = 'regex', case_insensitive = FALSE)

    expect_equal(
        predict(textmodel_newsmap(feat_dfm_de, label_dfm_de)),
        factor(c(text1 = "ie"), levels = "ie")
    )
})

test_that("test French dictionary and prediction work correctly", {

    txt_fr <- c("Ceci est un article sur l'Irlande.")
    toks_fr <- tokens(txt_fr)
    toks_fr <- tokens_split(toks_fr, "'")
    label_toks_fr <- tokens_lookup(toks_fr, data_dictionary_newsmap_fr, levels = 3)
    label_dfm_fr <- dfm(label_toks_fr)

    feat_dfm_fr <- dfm(toks_fr, tolower = FALSE) %>%
        dfm_select('^[A-Z][A-Za-z1-2]+', selection = "keep",
                   valuetype = 'regex', case_insensitive = FALSE)

    expect_equal(
        predict(textmodel_newsmap(feat_dfm_fr, label_dfm_fr)),
        factor(c(text1 = "ie"), levels = "ie")
    )
})

test_that("Hebrew dictionary and prediction work correctly", {

    skip_on_travis()
    txt_he <- c("טקסט על אירלנד.")
    toks_he <- tokens(txt_he)
    label_toks_he <- tokens_lookup(toks_he, data_dictionary_newsmap_he, levels = 3)
    label_dfm_he <- dfm(label_toks_he)
    feat_dfm_he <- dfm(toks_he, tolower = FALSE)

    expect_equal(
        predict(textmodel_newsmap(feat_dfm_he, label_dfm_he)),
        factor(c(text1 = "ie"), levels = "ie")
    )
})

test_that("Arabic dictionary and prediction work correctly", {

    skip_on_travis()
    txt_ar <- c("هذا مقال عن أيرلندا.")
    toks_ar <- tokens(txt_ar)
    label_toks_ar <- tokens_lookup(toks_ar, data_dictionary_newsmap_ar, levels = 3)
    label_dfm_ar <- dfm(label_toks_ar)
    feat_dfm_ar <- dfm(toks_ar, tolower = FALSE)

    expect_equal(
        predict(textmodel_newsmap(feat_dfm_ar, label_dfm_ar)),
        factor(c(text1 = "ie"), levels = "ie")
    )
})

test_that("Japanese dictionary and prediction work correctly", {

    skip_on_travis()
    txt_ja <- c("アイルランドに関するテキスト。")
    toks_ja <- tokens(txt_ja)
    label_toks_ja <- tokens_lookup(toks_ja, data_dictionary_newsmap_ja, levels = 3)
    label_dfm_ja <- dfm(label_toks_ja)

    feat_dfm_ja <- dfm(toks_ja, tolower = FALSE) %>%
        dfm_select('^[ぁ-ん]+$', selection = "remove", valuetype = 'regex')

    expect_equal(
        predict(textmodel_newsmap(feat_dfm_ja, label_dfm_ja)),
        factor(c(text1 = "ie"), levels = "ie")
    )
})

test_that("Traditional Chinese dictionary and prediction work correctly", {

    skip_on_travis()
    txt_zh_tw <- c("這篇文章關於愛爾蘭。")
    toks_zh_tw <- tokens(txt_zh_tw)
    label_toks_zh_tw <- tokens_lookup(toks_zh_tw, data_dictionary_newsmap_zh_tw, levels = 3)
    label_dfm_zh_tw <- dfm(label_toks_zh_tw)
    feat_dfm_zh_tw <- dfm(toks_zh_tw, tolower = FALSE)

    expect_equal(
        predict(textmodel_newsmap(feat_dfm_zh_tw, label_dfm_zh_tw)),
        factor(c(text1 = "ie"), levels = "ie")
    )
})


test_that("Simplified Chinese dictionary and prediction work correctly", {

    skip_on_travis()
    txt_zh_cn <- c("这篇文章关於爱尔兰。")
    toks_zh_cn <- tokens(txt_zh_cn)
    label_toks_zh_cn <- tokens_lookup(toks_zh_cn, data_dictionary_newsmap_zh_cn, levels = 3)
    label_dfm_zh_cn <- dfm(label_toks_zh_cn)
    feat_dfm_zh_cn <- dfm(toks_zh_cn, tolower = FALSE)

    expect_equal(
        predict(textmodel_newsmap(feat_dfm_zh_cn, label_dfm_zh_cn)),
        factor(c(text1 = "ie"), levels = "ie")
    )
})


test_that("methods for textmodel_newsmap works correctly", {
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
                 factor(c("ie", "in", "ie", "ie"), levels = c("in", "ie")))
    expect_equal(unname(predict(map, rank = 2)),
                 factor(c("in", "ie", "in", "in"), levels = c("in", "ie")))
    expect_error(predict(map, rank = 0))

    # different prediction outputs agree
    pred_top <- predict(map, confidence.fit = TRUE)
    pred_all <- predict(map, type = 'all')
    expect_equivalent(pred_top$confidence.fit, apply(pred_all, 1, max))
    expect_equivalent(pred_top$confidence.fit[1], pred_top$confidence.fit[3])

    # print
    expect_output(
        print(map),
        paste0('(\n)',
               'Call:(\n)',
               'textmodel_newsmap\\(.*\\)(\n)')
    )

    expect_output(
        print(summary(map)),
        paste0('(\n)',
               'Call:(\n)',
               'textmodel_newsmap\\(.*\\)(\n)',
               '\n',
               'Labels:(\n)',
               '\\[1\\] "in" "ie"(\n)',
               '(\n)',
               'Data Dimension:(\n)',
               '\\[1\\] 4 7(\n)')
    )

})

test_that("textmodel_newsmap() raises error if dfm is empty", {
    dfmt1 <- dfm(tokens("a b c"))
    dfmt2 <- dfm(tokens("A"))
    expect_error(textmodel_newsmap(dfm_trim(dfmt1, min_termfreq = 10), dfmt2),
                 "x must have at least one non-zero feature")

    expect_error(textmodel_newsmap(dfmt1, dfm_trim(dfmt2, min_termfreq = 10)),
                 "y must have at least one non-zero feature")
})

test_that("predict() returns NA for documents without registered features", {

    text <- c("Ireland is famous for Guinness.",
              "Guinness began retailing in India in 2007.",
              "Cork is an Irish coastal city.",
              "Titanic departed Cork Harbour in 1912.")

    toks <- tokens(text)
    label_toks <- tokens_lookup(toks, data_dictionary_newsmap_en, levels = 3)
    label_dfm <- dfm(label_toks)

    dfmt_feat <- dfm(tokens(c("aa bb cc", "aa bb", "bb cc")))
    dfmt_label <- dfm(tokens(c("A", "B", "B")), tolower = FALSE)
    dfmt_new <- dfm(tokens(c("aa bb cc", "aa bb", "zz")))
    map <- textmodel_newsmap(dfmt_feat, dfmt_label)
    expect_equal(predict(map),
                 factor(c(text1 = "A", text2 = "B", text3 = "B")))
    expect_equal(predict(map, newdata = dfmt_new),
                 factor(c(text1 = "A", text2 = "B", text3 = NA)))
    pred <- predict(map, confidence.fit = TRUE, newdata = dfmt_new)
    expect_equal(pred$class,
                 factor(c(text1 = "A", text2 = "B", text3 = NA)))
    expect_equal(pred$confidence.fit,
                 c(0.018, 0.048, NA), tolerance = 0.01)
    expect_equal(predict(map, newdata = dfmt_new, rank = 2),
                 factor(c(text1 = "B", text2 = "A", text3 = NA)))
    expect_equal(as.numeric(predict(map, newdata = dfmt_new, type = "all")),
                 c(0.018, -0.048, NA, -0.018, 0.048, NA), tolerance = 0.01)

})

test_that("accuracy() is correct", {

    v1 <- c("c", NA,  "b", "a", "b", "c", "b", "b", "a", "c")
    v2 <- c("c", "b", "a", "a", "b", "c", "b", "b", "a", "c")

    accu <- accuracy(v1, v2)

    expect_equal(accu$tp, c(2, 3, 3))
    expect_equal(accu$fp, c(0, 1, 0))
    expect_equal(accu$tn, c(6, 5, 6))
    expect_equal(accu$fn, c(1, 0, 0))

    expect_identical(
        accu,
        accuracy(rev(v1), rev(v2))
    )
})



