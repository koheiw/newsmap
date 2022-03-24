
test_that("predict() returns NA for documents without registered features", {

    dfmt_feat <- dfm(tokens(c("aa bb cc", "aa bb", "bb cc")))
    dfmt_label <- dfm(tokens(c("A", "B", "B")), tolower = FALSE)
    dfmt_new <- dfm(tokens(c("aa bb cc", "aa bb", "zz")))
    map <- textmodel_newsmap(dfmt_feat, dfmt_label)
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

})
