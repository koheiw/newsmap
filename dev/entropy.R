
require(quanteda)
require(newsmap)

txt <- c("Ireland is famous for beer.",
         "Beer is popular in Ireland.",
         "Cork is an Irish coastal city.",
         "India is known for curry.",
         "Indian curry and beer go well.",
         "New Delhi is the capital of India")

toks <- tokens(txt)
label_toks <- tokens_lookup(toks, data_dictionary_newsmap_en, levels = 3)
label_dfm <- dfm(label_toks)

feat_dfm <- dfm(toks) %>%
    dfm_remove(stopwords()) %>%
    dfm_select('^[a-z1-2]+', selection = "keep", valuetype = 'regex')
map_lr <- textmodel_newsmap(feat_dfm, label_dfm, measure = "likelihood")
map_et <- textmodel_newsmap(feat_dfm, label_dfm, measure = "entropy")

map_lr$model
map_et$model

predict(map_lr, confidence.fit = TRUE)
predict(map_et, confidence.fit = TRUE)
