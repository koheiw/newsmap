require(quanteda)

test_that("textmodel_newsmap() works", {
    toks <- tokens(data_corpus_inaugural, remove_punct = TRUE) %>%
        tokens_remove(stopwords())
    dfmt <- dfm(toks)
    dfmt$Party <- factor(dfmt$Party)

    smat <- xtabs( ~ docid(dfmt) + dfmt$Party, sparse = TRUE)
    map1 <- textmodel_newsmap(dfmt, smat)
    expect_equal(names(coef(map1)), levels(dfmt$Party))
    expect_null(map1$weight)

    mat <- as.matrix(smat)
    map2 <- textmodel_newsmap(dfmt, mat)
    expect_equal(names(coef(map2)), levels(dfmt$Party))
    expect_null(map2$weight)

    expect_error(textmodel_newsmap(list(), smat))
    expect_error(textmodel_newsmap(dfmt, list()))
    expect_error(textmodel_newsmap(dfmt, NULL))

    # use entropy weighting
    map_loc <- textmodel_newsmap(dfmt, mat, entropy = "local")
    expect_identical(dim(map_loc$weight), dim(map_loc$model))
    expect_false(all(map_loc$weight[1,] == map_loc$weight[2,]))
    expect_equal(coef(map_loc, 10)[[1]],
                 head(sort(map_loc$weight[1,] * map_loc$model[1,], decreasing = TRUE), 10))

    map_avg <- textmodel_newsmap(dfmt, mat, entropy = "average")
    expect_identical(dim(map_avg$weight), dim(map_avg$model))
    expect_true(all(map_avg$weight[1,] == map_avg$weight[2,]))
    expect_equal(coef(map_avg, 10)[[1]],
                 head(sort(map_avg$weight[1,] * map_avg$model[1,], decreasing = TRUE), 10))

    map_glo <- textmodel_newsmap(dfmt, mat, entropy = "global")
    expect_identical(dim(map_glo$weight), dim(map_glo$model))
    expect_true(all(map_glo$weight[1,] == map_glo$weight[2,]))
    expect_equal(coef(map_glo, 10)[[1]],
                 head(sort(map_glo$weight[1,] * map_glo$model[1,], decreasing = TRUE), 10))

    expect_false(all(map_glo$weight[1,] == map_loc$weight[1,]))
    expect_false(all(map_loc$weight[1,] == map_avg$weight[1,]))
    expect_false(all(map_avg$weight[1,] == map_glo$weight[1,]))

})


test_that("English dictionary and prediction work correctly", {

    txt_en <- c("This is an article about Ireland.",
                "This is an article about the Great Britain.")
    toks_en <- tokens(txt_en)
    label_toks_en <- tokens_lookup(toks_en, data_dictionary_newsmap_en, levels = 3)
    label_dfm_en <- dfm(label_toks_en)

    feat_dfm_en <- dfm(toks_en, tolower = FALSE) %>%
        dfm_select('^[A-Z][A-Za-z1-2]+', selection = "keep",
                             valuetype = 'regex', case_insensitive = FALSE)
    map_en <- textmodel_newsmap(feat_dfm_en, label_dfm_en)
    expect_equal(
        predict(map_en),
        factor(c(text1 = "ie", text2 = "gb"), levels = c("gb", "ie"))
    )
    expect_equivalent(
        names(coef(map_en)),
        c("gb", "ie")
    )
})


test_that("German dictionary and prediction work correctly", {

    txt_de <- c("Dies ist ein  Artikel über Irland.",
                "Dies ist ein Artikel über Großbritannien.")
    toks_de <- tokens(txt_de)
    label_toks_de <- tokens_lookup(toks_de, data_dictionary_newsmap_de, levels = 3)
    label_dfm_de <- dfm(label_toks_de)

    feat_dfm_de <- dfm(toks_de, tolower = FALSE) %>%
        dfm_select('^[A-Z][A-Za-z1-2]+', selection = "keep",
                             valuetype = 'regex', case_insensitive = FALSE)
    map_de <- textmodel_newsmap(feat_dfm_de, label_dfm_de)
    expect_equal(
        predict(map_de),
        factor(c(text1 = "ie", text2 = "gb"), levels = c("gb", "ie"))
    )
    expect_equivalent(
        names(coef(map_de)),
        c("gb", "ie")
    )
})

test_that("test French dictionary and prediction work correctly", {

    txt_fr <- c("Ceci est un article sur l'Irlande.",
                "Ceci est un article sur la Grande-Bretagne.")
    toks_fr <- tokens(txt_fr)
    toks_fr <- tokens_split(toks_fr, "'")
    label_toks_fr <- tokens_lookup(toks_fr, data_dictionary_newsmap_fr, levels = 3)
    label_dfm_fr <- dfm(label_toks_fr)

    feat_dfm_fr <- dfm(toks_fr, tolower = FALSE) %>%
        dfm_select('^[A-Z][A-Za-z1-2]+', selection = "keep",
                   valuetype = 'regex', case_insensitive = FALSE)
    map_fr <- textmodel_newsmap(feat_dfm_fr, label_dfm_fr)
    expect_equal(
        predict(map_fr),
        factor(c(text1 = "ie", text2 = "gb"), levels = c("gb", "ie"))
    )
    expect_equivalent(
        names(coef(map_fr)),
        c("gb", "ie")
    )
})

test_that("Hebrew dictionary and prediction work correctly", {

    skip_on_travis()
    txt_he <- c("טקסט על אירלנד.",
                "זה מאמר על בריטניה הגדולה.")
    toks_he <- tokens(txt_he)
    label_toks_he <- tokens_lookup(toks_he, data_dictionary_newsmap_he, levels = 3)
    label_dfm_he <- dfm(label_toks_he)
    feat_dfm_he <- dfm(toks_he, tolower = FALSE)
    map_he <- textmodel_newsmap(feat_dfm_he, label_dfm_he)
    expect_equal(
        predict(map_he),
        factor(c(text1 = "ie", text2 = "gb"), levels = c("gb", "ie"))
    )
    expect_equivalent(
        names(coef(map_he)),
        c("gb", "ie")
    )
})

test_that("Arabic dictionary and prediction work correctly", {

    skip_on_travis()
    txt_ar <- c("هذا مقال عن أيرلندا.",
                "هذا مقال عن بريطانيا العظمى.")
    toks_ar <- tokens(txt_ar)
    label_toks_ar <- tokens_lookup(toks_ar, data_dictionary_newsmap_ar, levels = 3)
    label_dfm_ar <- dfm(label_toks_ar)
    feat_dfm_ar <- dfm(toks_ar, tolower = FALSE)
    map_ar <- textmodel_newsmap(feat_dfm_ar, label_dfm_ar)
    expect_equal(
        predict(map_ar),
        factor(c(text1 = "ie", text2 = "gb"), levels = c("gb", "ie"))
    )
    expect_equivalent(
        names(coef(map_ar)),
        c("gb", "ie")
    )
})

test_that("Japanese dictionary and prediction work correctly", {

    skip_on_travis()
    txt_ja <- c("これはアイルランドに関する記事です。",
                "これは英国に関する記事です。")
    toks_ja <- tokens(txt_ja)
    label_toks_ja <- tokens_lookup(toks_ja, data_dictionary_newsmap_ja, levels = 3)
    label_dfm_ja <- dfm(label_toks_ja)

    feat_dfm_ja <- dfm(toks_ja, tolower = FALSE) %>%
        dfm_select('^[ぁ-ん]+$', selection = "remove", valuetype = 'regex')
    map_ja <- textmodel_newsmap(feat_dfm_ja, label_dfm_ja)
    expect_equal(
        predict(map_ja),
        factor(c(text1 = "ie", text2 = "gb"), levels = c("gb", "ie"))
    )
    expect_equivalent(
        names(coef(map_ja)),
        c("gb", "ie")
    )
})

test_that("Traditional Chinese dictionary and prediction work correctly", {

    skip_on_travis()
    txt_zh_tw <- c("這篇文章關於愛爾蘭。",
                   "這篇文章關於英國的文章。")
    toks_zh_tw <- tokens(txt_zh_tw)
    label_toks_zh_tw <- tokens_lookup(toks_zh_tw, data_dictionary_newsmap_zh_tw, levels = 3)
    label_dfm_zh_tw <- dfm(label_toks_zh_tw)
    feat_dfm_zh_tw <- dfm(toks_zh_tw, tolower = FALSE)
    map_zh_tw <- textmodel_newsmap(feat_dfm_zh_tw, label_dfm_zh_tw)
    expect_equal(
        predict(map_zh_tw),
        factor(c(text1 = "ie", text2 = "gb"), levels = c("gb", "ie"))
    )
    expect_equivalent(
        names(coef(map_zh_tw)),
        c("gb", "ie")
    )
})


test_that("Simplified Chinese dictionary and prediction work correctly", {

    skip_on_travis()
    txt_zh_cn <- c("这篇文章关於爱尔兰。",
                   "这篇文章关于英国的文章。")
    toks_zh_cn <- tokens(txt_zh_cn)
    label_toks_zh_cn <- tokens_lookup(toks_zh_cn, data_dictionary_newsmap_zh_cn, levels = 3)
    label_dfm_zh_cn <- dfm(label_toks_zh_cn)
    feat_dfm_zh_cn <- dfm(toks_zh_cn, tolower = FALSE)
    map_zh_cn <- textmodel_newsmap(feat_dfm_zh_cn, label_dfm_zh_cn)
    expect_equal(
        predict(map_zh_cn),
        factor(c(text1 = "ie", text2 = "gb"), levels = c("gb", "ie"))
    )
    expect_equivalent(
        names(coef(map_zh_cn)),
        c("gb", "ie")
    )
})


test_that("methods for textmodel_newsmap works correctly", {
    txt <- c("Ireland is famous for Guinness.",
              "Guinness began retailing in India in 2007.",
              "Cork is an Irish coastal city.",
              "Titanic departed Cork Harbour in 1912.")

    toks <- tokens(txt)
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
               'textmodel_newsmap.dfm\\(.*\\)(\n)')
    )

    expect_output(
        print(summary(map)),
        paste0('(\n)',
               'Call:(\n)',
               'textmodel_newsmap.dfm\\(.*\\)(\n)',
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

test_that("label is working", {
    txt <- c("American and Japanese leaders met in Tokyo.",
             "Paris Hilton visited British museum in London.",
             "India and Pakistan are neighbours.",
             "A man went to the Moon.")
    toks <- tokens(txt)
    toks_label <- tokens_lookup(toks, data_dictionary_newsmap_en, levels = 3)
    dfmt <- dfm(toks)
    dfmt_label <- dfm(toks_label)

    map1 <- textmodel_newsmap(dfmt, dfmt_label)
    expect_equal(names(coef(map1)), c("us", "jp", "in", "pk", "gb", "fr"))

    map2 <- textmodel_newsmap(dfmt, dfmt_label, label = "max")
    expect_equal(names(coef(map2)), c("jp", "in", "pk", "gb"))
})
