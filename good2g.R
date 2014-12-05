ng2 <- function(data) {
    #subsetting the data we want
    word <- data
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    #cleaning puncuation and capitalization
    word <- gsub("([\\.\\?!])", "<>", word) #replace . ? ! with <>
    word <- gsub("<>", "~<>~", word) #split at <>
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    # collapsing and ngramming
    allone <- paste(word, collapse = '')
    allw <- txt.to.words(allone, splitting.rule = "[ ~]", preserve.case = T)
    ##filter out profanity
    # allw <- allw[-grep("fuck|shit|damn|crap|bitch", allw, ignore.case=T)]
    ngrams <- make.ngrams(allw, 2)
    #compiling counts of ngrams
    dcount <- data.frame(table(ngrams))
    #getting rid of stupid ngrams
    ##dealing with dashes
    dashi <- grep("-", dcount$ngrams)
    gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$ngrams)
    dmatch <- match(gdashi, dashi)
    bdashi <- dashi[-dmatch]
    dcount <- dcount[-bdashi, ]
    #ranking ngrams by Freq
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    #splitting them for prediction
    dcount$ngrams <- as.character(dcount$ngrams)
    sn <- strsplit(dcount$ngrams, " ")
    dcount$w1 <- sapply(sn, function(sn){sn[1]})
    dcount$w2 <- sapply(sn, function(sn){sn[2]})
    ##getting rid of random unpredictable puncuation
    dcount <- dcount[-grep("<>", dcount$w2),]
    dcount
}

ng2toke <- function(data) {
    #subsetting the data we want
    word <- data
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    #cleaning puncuation and capitalization
    word <- gsub("([\\.\\?!])", "<>", word) #replace . ? ! with <>
    word <- gsub("<>", "~<>~", word) #split at <>
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    # collapsing and ngramming
    allone <- paste(word, collapse = '')
    word <- txt.to.words(allone, splitting.rule = "[ ~]", preserve.case = T)
    ##filter out profanity
    # word <- word[-grep("fuck|shit|damn|crap|bitch", word, ignore.case=T)]
    dcount <- data.frame(table(word))
    #getting rid of stupid ngrams
    ##dealing with dashes
    dashi <- grep("-", dcount$word)
    gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$word)
    dmatch <- match(gdashi, dashi)
    bdashi <- dashi[-dmatch]
    dcount <- dcount[-bdashi, ]
    #ranking ngrams by Freq
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount
}

good2g <- function(ng, tk){
    names(tk)[2] <- 'tkfreq'
    ng <- merge(ng,tk,by.x = 'w2',by.y = 'word',all.x = T)
    ng <- merge(ng,tk,by.x = 'w1',by.y = 'word',all.x = T)
    ng$tkfreq.x[is.na(ng$tkfreq.x)] <- 0
    ng$tkfreq.y[is.na(ng$tkfreq.y)] <- 0
    ng$dnm <- ng$tkfreq.x + ng$tkfreq.y
    ng$rate <- ng$Freq / ng$dnm
    ng[is.infinite(ng$rate),'rate'] <- 1
    goodng <- ng[ng$rate>.15, c("ngrams", "Freq", "dnm", "rate")]
    goodng <- goodng[order(goodng$Freq, decreasing=T),]
    goodng
}

