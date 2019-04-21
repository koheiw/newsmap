require(quanteda)

quanteda_options(threads = 8)
corp <- readRDS("/home/kohei/Documents/US Politics/Data/data_corpus_asahi_q10.RDS")
ndoc(corp)

toks <- tokens(corp)
col <- tokens_select(toks, '^[ァ-ヶー一-龠]+$', valuetype = 'regex', padding = TRUE) %>%
    textstat_collocations(min_count = 100)

toks_comp <- tokens_compound(toks, col)

dict_new <- dictionary(file = "./dict/japanese2.yml")
dict_old <- dictionary(file = "./dict/japanese.yml")

n_new <- colSums(dfm(tokens_lookup(toks, dict_new, levels = 3)))
n_old <- colSums(dfm(tokens_lookup(toks, dict_old, levels = 3)))
diff <- (n_new - n_old) / n_old

n_new2 <- colSums(dfm(tokens_lookup(toks_comp, dict_new, levels = 3)))
n_old2 <- colSums(dfm(tokens_lookup(toks_comp, dict_old, levels = 3)))
diff2 <- (n_new2 - n_old2) / n_old2

par(mfrow = c(1, 2))
plot(diff, type = "n", main = "Non-compounded")
text(seq_along(diff), diff, ifelse(abs(diff) > 0.1, names(diff), NA))
abline(h = 0)

plot(diff2, type = "n", main = "Compounded")
text(seq_along(diff2), diff2, ifelse(abs(diff2) > 0.1, names(diff2), NA))
abline(h = 0)
par(mfrow = c(1, 1))
