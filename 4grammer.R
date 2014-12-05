## THE CONSTRUCTOR
### works, but have to do it in chunks of 500k tweets or 200k articles/blogs
FG1 <- function(data) {
    #subsetting the data we want
    word <- data
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    #cleaning puncuation and capitalization
    word <- gsub("([\\.\\?!])", "<>", word) #replace . ? ! with <>
    word <- gsub("<>", "~<>~", word) #split at <>
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    # collapsing and ngramming
    allone <- paste(word, collapse = ' ')
    allw <- txt.to.words(allone, splitting.rule = "[ ~]", preserve.case = T)
    #getting rid of stupid words
    ##filter out profanity
    allw <- allw[-grep("fuck|shit|damn|crap|bitch", allw, ignore.case=T)]
    ##dealing with dashes
    dashi <- grep("-", allw)
    gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", allw)
    dmatch <- match(gdashi, dashi)
    bdashi <- dashi[-dmatch]
    allw <- allw[-bdashi]
    ##getting rid of hashtags
    allw <- allw[-grep("#[A-Za-z0-9]+", allw)]
    #make ngrams
    ngrams <- make.ngrams(allw, 4)
    #compiling counts of ngrams
    dcount <- data.frame(table(ngrams))
    #get rid of singletons
    dcount <- dcount[dcount$Freq>1,]
    dcount
}

FG2 <- function(dcount){
    #aggregate Freq sums
    dcount <- aggregate(dcount$Freq, list(dcount$ngrams), FUN="sum")
    names(dcount) <- c("ngrams", "Freq")
    #ranking ngrams by Freq
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    #splitting them for prediction
    dcount$ngrams <- as.character(dcount$ngrams)
    sn <- strsplit(dcount$ngrams, " ")
    dcount$w1 <- sapply(sn, function(sn){sn[1]})
    dcount$w2 <- sapply(sn, function(sn){sn[2]})
    dcount$w3 <- sapply(sn, function(sn){sn[3]})
    dcount$w4 <- sapply(sn, function(sn){sn[4]})
    ##getting rid of random unpredictable puncuation
    dcount <- dcount[-grep("<>", dcount$w2),]
    dcount <- dcount[-grep("<>", dcount$w3),]
    dcount <- dcount[-grep("<>", dcount$w4),]
    dcount
}

set.seed(3030)
blogTrain <- round(runif(length(blogs)*.75, 1, length(blogs)), 0)
blogTr <- blogs[blogTrain]
blogTe <- blogs[-blogTrain]

blogTrFG1 <- FG1(blogTr) # took about 20 minutes
blogTrFG <- FG2(blogTrFG1) # took another 20 minutes
write.csv(blogTrFG, "blogTrFGtry1.csv")
write.csv(blogTrFG[1:500000,], "blogTrFG500k.csv")

simpleFGpredictor <- function(word1, word2, word3){
    if(length(grep(word1, gcount$w1))>0){
        preds <- gcount[grep(word1, gcount$w1), ]
    }else{preds <- gcount}
    if(length(grep(word2, preds$w2))>0){
        preds <- preds[grep(word2, preds$w2), ]
    }else{preds <- preds}
    if(length(grep(word3, preds$w3))>0){
        preds <- preds[grep(word3, preds$w3), ]
    }else{preds <- preds}
    print(preds$w4[1:3]) 
}

FGpredictor <- function(word1, word2, word3){
    gcount <- blogTrFG500k
    podium <- data.frame(word=c("","",""), score=c(0,0,0), stringsAsFactors=F)
    if(length(grep(word1, gcount$w1))>0){
        preds <- gcount[grep(word1, gcount$w1), ]
    }else{preds <- gcount}
    if(length(grep(word2, preds$w2))>0){
        preds <- preds[grep(word2, preds$w2), ]
    }else{preds <- preds}
    if(length(grep(word3, preds$w3))>0){
        preds <- preds[grep(word3, preds$w3), ]
    }else{preds <- preds}
    podium$word[1:3] <- preds$w4[1:3]
    podium$score[1:3] <- preds$Freq[1:3]
    podium
}

# combine with 3grammer (redesign) and 2grammer (build/work of 2g)
# then fix the podium
# then make the score Freq/sum(data$Freq) 
###but save sum as a number so it doesn't try to sum each time
# do other things to improve efficiency

