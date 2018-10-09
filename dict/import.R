require(quanteda)

data_dictionary_newsmap_en <- dictionary(file = 'dict/english.yml')
save(data_dictionary_newsmap_en, file = 'data/data_dictionary_newsmap_en.RData')

data_dictionary_newsmap_ja <- dictionary(file = 'dict/japanese.yml')
save(data_dictionary_newsmap_ja, file = 'data/data_dictionary_newsmap_ja.RData')

data_dictionary_newsmap_de <- dictionary(file = 'dict/german.yml')
save(data_dictionary_newsmap_de, file = 'data/data_dictionary_newsmap_de.RData')

data_dictionary_newsmap_es <- dictionary(file = 'dict/spanish.yml')
save(data_dictionary_newsmap_es, file = 'data/data_dictionary_newsmap_es.RData')

data_dictionary_newsmap_ru <- dictionary(file = 'dict/russian.yml')
save(data_dictionary_newsmap_ru, file = 'data/data_dictionary_newsmap_ru.RData')

data_dictionary_newsmap_zh <- dictionary(file = 'dict/chinese.yml')
save(data_dictionary_newsmap_zh, file = 'data/data_dictionary_newsmap_zh.RData')
