
#' Construct a geographical dictionary from a list of place names
#' @param tokens tokenizedTexts object
#' @param dict quanteda's dictionary object
#' @param smooth smoother for coditional frequency
#' @param sep_keyword separator for keywords in lexicon
#' @export
construct <- function(toks, dict, concatenator = ' ', weight = 1, smooth = 1, min_count_seq = 2){

    if(!is.dictionary(dict)) stop("Dictionary has to be a quanteda dictionary object")
    if(length(dict) == 0) stop("Dictionary has not entry")

    # Dicitonary lookup
    cat("Searching for dictionary words...\n")
    keys <- quanteda::tokens_lookup(toks, dict, levels = 3)
    mx_key <- quanteda::dfm(keys, tolower = FALSE)

    # Feature selection
    md5 <- digest::digest(toks , 'md5')
    file_seqs <- paste0('.cache_', min_count_seq, '_', md5 ,'.RDS', collapse = '')
    cat("Identifying multi-word names...\n")
    if (file.exists(file_seqs)) {
        cat("Loading cached multi-word names:", file_seqs, "\n")
        seqs <- readRDS(file=file_seqs)
    } else {
        seqs <- quanteda::sequences(toks, '^[A-Z]', valuetype = 'regex', case_insensitive = FALSE,
                                    min_count = min_count_seq)
        saveRDS(seqs, file=file_seqs)
    }

    cat("Concatenating multi-word names...\n")
    seqs <- seqs[seqs$p < 0.001,]
    toks <- quanteda::tokens_compound(toks, seqs$sequence, valuetype = 'fixed', concatenator = ' ')
    toks <- quanteda::tokens_select(toks, '^[A-Z]', valuetype = 'regex', padding = TRUE, case_insensitive = FALSE)
    mx <- dfm(toks)
    mx <- dfm_select(mx, min_len = 2)
    mx <- quanteda::dfm_toupper(mx)

    mx_dic <- matrix(rep(0, ncol(mx) * ncol(mx_key)), ncol=ncol(mx), nrow=ncol(mx_key))
    colnames(mx_dic) <- colnames(mx)
    rownames(mx_dic) <- colnames(mx_key)

    cat("Scoring words for countries: ")
    for(key in sort(featnames(mx_key))){

        cat(key, " ", sep = "")
        rownames(mx) <- ifelse(as.vector(mx_key[,key]) > 0, 'T', 'R')
        mx2 <- quanteda::dfm_compress(mx, margin = 'documents')

        missing <- setdiff(c('T', 'R'), rownames(mx2))
        attr(mx2, 'Dim')[1L] <- attr(mx2, 'Dim')[1L] + length(missing)
        attr(mx2, 'Dimnames')$docs <- c(attr(mx2, 'Dimnames')$docs, missing)

        # Scoring words
        mx2 <- mx2 + smooth
        sums <- Matrix::rowSums(mx2)
        if (sums['T'] > 1) {
            mx3 <- (mx2 ^ weight) / (sums) # conditional likelihood
            lr <- as.vector(log(mx3['T',]) - log(mx3['R',])) # calculate likelihood-ratio
            mx_dic[key,] <- lr
        } else {
            mx_dic[key,] <- NULL
        }
    }
    cat("\n")
    mx_dict_sps <- as(mx_dic, "sparseMatrix")
    return(mx_dict_sps)
}

#' @export
top_entries <-function(dict, code, n=20){
    ents <- dict[code,]
    ents <- ents[order(ents, decreasing = TRUE)]
    print(head(ents, n))
    cat(sum(ents > 0), 'non-zero entries\n')
}

