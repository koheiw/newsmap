require(quanteda)

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
