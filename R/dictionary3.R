
#' Construct a geographical dictionary from a list of place names
#' @param tokens tokenizedTexts object
#' @param dict quanteda's dictionary object
#' @param smooth smoother for coditional frequency
#' @param sep_keyword separator for keywords in lexicon
#' @export
make_dictionary <- function(toks, dict, concatenator = ' ', weight = 1, smooth = 10e-5){

    if(!is.dictionary(dict)) stop("Dictionary has to be a quanteda dictionary object")
    if(length(dict) == 0) stop("Dictionary has not entry")

    # Dicitonary lookup
    cat("Searching for dictionary words\n")
    keys <- quanteda::tokens_lookup(toks, dict, levels = 3)
    mx_key <- quanteda::dfm(keys, tolower = FALSE)

    # Feature selection
    file_seqs <- 'sequences_capital.RDS'

    # if (file.exists(file_seqs)) {
    #   cat("Loading cached multi-word names:", file_seqs, "\n")
    #   seqs <- readRDS(file=file_seqs)
    # } else {
    #   cat("Idenityfing multi-word names\n")
    seqs <- quanteda::sequences(toks, '^[A-Z]', valuetype = 'regex', case_insensitive = FALSE, min_count = 2)
    saveRDS(seqs, file=file_seqs)
    # }
    toks <- quanteda::tokens_compound(toks, seqs$sequence, valuetype = 'fixed', concatenator = ' ')
    toks <- quanteda::tokens_select(toks, '^[A-Z]', valuetype = 'regex', padding = TRUE, case_insensitive = FALSE)
    mx <- dfm(toks)
    mx <- dfm_select(mx, min_len = 2)
    mx <- quanteda::dfm_toupper(mx)

    mx_dic <- matrix(rep(0, ncol(mx) * ncol(mx_key)), ncol=ncol(mx), nrow=ncol(mx_key))
    colnames(mx_dic) <- colnames(mx)
    rownames(mx_dic) <- colnames(mx_key)

    for(key in sort(featnames(mx_key))){

        cat(key, "\n")
        rownames(mx) <- ifelse(as.vector(mx_key[,key]) > 0, 'T', 'R')
        mx2 <- quanteda::dfm_compress(mx, margin = 'documents')

        missing <- setdiff(c('T', 'R'), rownames(mx2))
        attr(mx2, 'Dim')[1L] <- attr(mx2, 'Dim')[1L] + length(missing)
        attr(mx2, 'Dimnames')$docs <- c(attr(mx2, 'Dimnames')$docs, missing)
        #flag_zero <- as.vector(mx2['T',] == 0) # flag words do not occur

        # Scoring words

        #sum <- sum(mx2) + 1
        mx2 <- mx2 + 1
        sums <- Matrix::rowSums(mx2)

        if (sums['T'] > 1) {
            #mx3 <- (mx2 ^ weight) / sums # conditional likelihood
            #if (missing(smooth)) smooth <- sums['T'] / sum #
            #cat("sums\n")
            #print(sums)
            #print(smooth)
            #mx3 <- ((mx2 ^ weight) + smooth) / (sums + smooth) # conditional likelihood
            mx3 <- (mx2 ^ weight) / (sums) # conditional likelihood
            print(head(mx3, 20))
            lr <- as.vector(log(mx3['T',]) - log(mx3['R',])) # # calculate likelihood-ratio
            #lr[flag_zero] <- 0 # fill with zero if words do no occur
            mx_dic[key,] <- lr
        } else {
            mx_dic[key,] <- NULL
        }
    }
    mx_dict_sps <- as(mx_dic, "sparseMatrix")
    return(mx_dict_sps)
}

#' @export
topEntries <-function(dict, code, n=20){
  ents <- dict[code,]
  ents <- ents[order(ents, decreasing = TRUE)]
  print(head(ents, n))
  cat(sum(ents > 0), 'non-zero entries\n')
}

