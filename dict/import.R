require(quanteda)

data_dictionary_newsmap_en <- dictionary(file = 'dict/english.yml')
save(data_dictionary_newsmap_en, file = 'data/data_dictionary_newsmap_en.RData')

data_dictionary_newsmap_ja <- dictionary(file = 'dict/japanese.yml')
save(data_dictionary_newsmap_ja, file = 'data/data_dictionary_newsmap_ja.RData')

data_dictionary_newsmap_de <- dictionary(file = 'dict/german.yml')
save(data_dictionary_newsmap_de, file = 'data/data_dictionary_newsmap_de.RData')

data_dictionary_newsmap_fr <- dictionary(file = 'dict/french.yml')
save(data_dictionary_newsmap_fr, file = 'data/data_dictionary_newsmap_fr.RData')

data_dictionary_newsmap_es <- dictionary(file = 'dict/spanish.yml')
save(data_dictionary_newsmap_es, file = 'data/data_dictionary_newsmap_es.RData')

data_dictionary_newsmap_pt <- dictionary(file = 'dict/portuguese.yml')
save(data_dictionary_newsmap_pt, file = 'data/data_dictionary_newsmap_pt.RData')

data_dictionary_newsmap_ru <- dictionary(file = 'dict/russian.yml')
save(data_dictionary_newsmap_ru, file = 'data/data_dictionary_newsmap_ru.RData')

data_dictionary_newsmap_it <- dictionary(file = 'dict/italian.yml')
save(data_dictionary_newsmap_it, file = 'data/data_dictionary_newsmap_it.RData')

data_dictionary_newsmap_he <- dictionary(file = 'dict/hebrew.yml')
save(data_dictionary_newsmap_he, file = 'data/data_dictionary_newsmap_he.RData')

data_dictionary_newsmap_ar <- dictionary(file = 'dict/arabic.yml')
save(data_dictionary_newsmap_ar, file = 'data/data_dictionary_newsmap_ar.RData')

data_dictionary_newsmap_tr <- dictionary(file = 'dict/turkish.yml')
save(data_dictionary_newsmap_tr, file = 'data/data_dictionary_newsmap_tr.RData')

data_dictionary_newsmap_zh_cn <- dictionary(file = 'dict/chinese_simplified.yml')
save(data_dictionary_newsmap_zh_cn, file = 'data/data_dictionary_newsmap_zh_cn.RData')

data_dictionary_newsmap_zh_tw <- dictionary(file = 'dict/chinese_traditional.yml')
save(data_dictionary_newsmap_zh_tw, file = 'data/data_dictionary_newsmap_zh_tw.RData')
