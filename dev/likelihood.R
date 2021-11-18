require(quanteda)
require(newsmap)

txt <- c("Ireland is famous for beer.",
         "Beer is popular in Ireland.",
         "Cork is an Irish coastal city.",
         "India is known for curry.",
         "Indian curry and beer go well.",
         "New Delhi is the capital of India")

toks <- tokens(txt)
dfmt_label <- dfm(tokens_lookup(toks, data_dictionary_newsmap_en, levels = 3))
dfmt_feat <- dfm(toks, tolower = FALSE) %>%
    dfm_select('^[A-Z][A-Za-z1-2]+', selection = "keep",
               valuetype = 'regex', case_insensitive = FALSE)

map1 <- textmodel_newsmap(dfmt_feat, dfmt_label)
map2 <- textmodel_newsmap(dfmt_feat, dfmt_label, measure = "likelihood")
map3 <- textmodel_newsmap(dfmt_feat, dfmt_label, measure = "likelihood", smooth = 0.001)

map1$model
map2$model
map3$model

