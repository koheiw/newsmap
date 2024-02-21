require(quanteda)

toks_test <- tokens(data_corpus_inaugural, remove_punct = TRUE)
dfmt_test <- dfm(toks_test) %>%
    dfm_remove(stopwords("en"))
toks_dict_test <- tokens_lookup(toks_test, data_dictionary_newsmap_en, level = 3)
dfmt_dict_test <- dfm(toks_dict_test)

test_that("textmodel_newsmap() works with different inputs", {
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
    expect_warning(textmodel_newsmap(dfmt, mat, aaa = 10),
                   "aaa argument is not used")

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

    expect_equal(
        names(map),
        c("model", "entropy", "data", "weight", "feature",
          "concatenator", "call", "version")
    )

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
    pred_top <- predict(map, confidence = TRUE)
    pred_all <- predict(map, type = 'all')
    expect_equivalent(pred_top$confidence.fit, apply(pred_all, 1, max))
    expect_equivalent(pred_top$confidence.fit[1], pred_top$confidence.fit[3])

    expect_warning(
        predict(map, confidence.fit = TRUE),
        "'confidence.fit' is deprecated; use 'confidence'"
    )

    expect_output(
        textmodel_newsmap(feat_dfm, label_dfm, verbose = TRUE),
        'Fitting textmodel_newsmap.*label = "ie".*  label = "in"'
    )

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

test_that("label and drop_label are working", {
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

    map3 <- textmodel_newsmap(dfmt, dfmt_label, drop_label = FALSE)
    expect_equal(names(coef(map3)), colnames(dfmt_label))
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

test_that("afe() is working", {

    txt <- c("American and Japanese leaders met in Tokyo.",
             "Paris Hilton visited British museum in London.",
             "India and Pakistan are neighbours.",
             "A man went to the Moon.")
    toks <- tokens(txt)
    toks_label <- tokens_lookup(toks, data_dictionary_newsmap_en, levels = 3)
    dfmt <- dfm(toks)
    dfmt_label <- dfm(toks_label)

    expect_equal(afe(dfmt, dfmt_label),
                 7.90, tolerance = 0.1)
    expect_error(afe(dfmt, matrix()))
    expect_error(afe(list(), dfmt_label))
})

test_that("coef() and dictionary() are working", {

    dfmt <- dfm_trim(dfmt_test, min_termfreq = 100)
    dfmt_dict <- dfm_trim(dfmt_dict_test, min_termfreq = 2)

    map <- textmodel_newsmap(dfmt, dfmt_dict)

    expect_true(all(lengths(coef(map, n = 5)) == 5))
    expect_identical(coef(map)[c("us")],
                     coef(map, select = c("us")))
    expect_identical(coef(map)[c("us", "ru", "gb")],
                     coef(map, select = c("us", "gb", "ru")))
    expect_identical(coef(map, select = c("ru", "gb", "us")),
                     coef(map, select = c("us", "gb", "ru")))

    expect_error(coef(map, n = -1),
                 "The value of n must be between 0 and Inf")
    expect_error(coef(map, select = "xx"),
                 "Selected class must be in the model")
    expect_error(coef(map, select = character()),
                 "The length of select must be between 1 and 16")

    # TODO: remove as.list()
    lis1 <- as.list(map)
    expect_equal(length(lis1), 16)
    expect_true(all(sapply(lis1, is.character)))
    expect_true(all(lengths(as.list(lis1)) == 10))

    lis2 <- as.list(map, n = 20, c("ru", "fr"))
    expect_equal(names(lis2), c("ru", "fr"))
    expect_true(all(sapply(lis2, is.character)))
    expect_true(all(lengths(lis2) == 20))

    # TODO: change to as.dictionary()
    dict1 <- newsmap:::as.dictionary.textmodel_newsmap(map)
    expect_s4_class(dict1, "dictionary2")
    expect_true(all(lengths(as.list(dict1)) == 10))

    dict2 <- newsmap:::as.dictionary.textmodel_newsmap(map, n = 20, c("ru", "fr"))
    expect_s4_class(dict2, "dictionary2")
    expect_equal(names(dict2), c("ru", "fr"))
    expect_true(all(lengths(dict2) == 20))

})
