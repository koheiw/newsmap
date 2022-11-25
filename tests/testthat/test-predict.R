require(quanteda)

test_that("predict() is working", {

    dfmt_feat <- dfm(tokens(c("aa bb cc", "aa bb", "bb cc")))
    dfmt_label <- dfm(tokens(c("A", "B", "B")), tolower = FALSE)
    dfmt_new <- dfm(tokens(c("aa bb cc", "aa bb", "zz")))
    map <- textmodel_newsmap(dfmt_feat, dfmt_label)

    expect_equal(
        predict(map),
        factor(c(text1 = "A", text2 = "B", text3 = "B"), levels = c("A", "B"))
    )
    expect_equal(
        predict(map, rank = 2),
        factor(c(text1 = "B", text2 = "A", text3 = "A"), levels = c("A", "B"))
    )
    expect_error(
        predict(map, rank = -1),
        "The value of rank must be between 1 and 2"
    )
    expect_error(
        predict(map, rank = 1:2),
        "The length of rank must be 1"
    )

    # NA for documents without registered feature
    expect_equal(predict(map),
                 factor(c(text1 = "A", text2 = "B", text3 = "B")))
    expect_equal(predict(map, newdata = dfmt_new),
                 factor(c(text1 = "A", text2 = "B", text3 = NA)))
    pred <- predict(map, confidence = TRUE, newdata = dfmt_new)
    expect_equal(pred$class,
                 factor(c(text1 = "A", text2 = "B", text3 = NA)))
    expect_equal(pred$confidence.fit,
                 c(0.018, 0.048, NA), tolerance = 0.01)
    expect_equal(predict(map, newdata = dfmt_new, rank = 2),
                 factor(c(text1 = "B", text2 = "A", text3 = NA)))
    expect_equal(as.numeric(predict(map, newdata = dfmt_new, type = "all")),
                 c(0.018, -0.048, NA, -0.018, 0.048, NA), tolerance = 0.01)

})

test_that("min_n is working", {

    dfmt_feat <- dfm(tokens(c("aa bb cc dd", "aa bb", "bb cc")))
    dfmt_label <- dfm(tokens(c("A", "B", "B")), tolower = FALSE)
    map <- textmodel_newsmap(dfmt_feat, dfmt_label)
    pred1 <- predict(map, type = "all")

    pred2 <- predict(map, type = "all", min_n = 1)
    expect_equal(pred2, pred1)

    pred3 <- predict(map, type = "all", min_n = 10)
    expect_equal(pred3, pred1 * ntoken(dfmt_feat) / 10)

    pred4 <- predict(map, type = "all", min_n = 3)
    expect_equal(pred4, pred1 * ntoken(dfmt_feat) / c(4, 3, 3))

    expect_error(
        predict(map, type = "all", min_n = 1:2),
        "The length of min_n must be 1"
    )
    expect_error(
        predict(map, type = "all", min_n = -3),
        "The value of min_n must be between 0 and Inf"
    )

})


test_that("min_conf is working", {

    dfmt_feat <- dfm(tokens(c("aa bb cc dd", "aa bb", "bb cc")))
    dfmt_label <- dfm(tokens(c("A", "B", "B")), tolower = FALSE)
    map <- textmodel_newsmap(dfmt_feat, dfmt_label)

    pred1 <- predict(map, confidence = TRUE)
    expect_equal(
        pred1$class,
        factor(c(text1 = "A", text2 = "B", text3 = "B"), levels = c("A", "B"))
    )

    pred2 <- predict(map, confidence = TRUE, min_conf = 0.1)
    expect_equal(
        pred2$class,
        factor(c(text1 = NA, text2 = "B", text3 = "B"), levels = c("A", "B"))
    )

    pred3 <- predict(map, confidence = TRUE, min_conf = 1)
    expect_equal(
        pred3$class,
        factor(c(text1 = NA, text2 = NA, text3 = NA), levels = c("A", "B"))
    )

    expect_error(
        predict(map, confidence = TRUE, min_conf = NA),
        "The value of min_conf cannot be NA"
    )
    expect_error(
        predict(map, confidence = TRUE, min_conf = c(0.1, 0)),
        "The length of min_conf must be 1"
    )
})

