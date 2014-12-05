setwd("C:/Users/Seth/Documents/Coursera/Capstone")
library(stylo)
#library(tm)
#library(openNLP)


ng2p1 <- function(data) {
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
    ifelse(length(grep("fuck|shit|damn|crap|bitch", allw, ignore.case=T))>0, 
           allw <- allw[-grep("fuck|shit|damn|crap|bitch", allw, ignore.case=T)], 
           allw <- allw)
    ##dealing with dashes
    dashi <- grep("-", allw)
    gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", allw)
    dmatch <- match(gdashi, dashi)
    bdashi <- dashi[-dmatch]
    ifelse(length(bdashi)>0, 
           allw <- allw[-bdashi], 
           allw <- allw)
    ##getting rid of hashtags
    ifelse(length(grep("#[A-Za-z0-9]+", allw))>0, 
           allw <- allw[-grep("#[A-Za-z0-9]+", allw)], 
           allw <- allw)
    #make ngrams
    ngrams <- make.ngrams(allw, 2)
    #compiling counts of ngrams
    dcount <- data.frame(table(ngrams))
    ##getting rid of puncuation
    dcount <- dcount[-grep("<>", dcount$ngrams),]
    #get rid of singletons
    dcount <- dcount[dcount$Freq>3,]
    #ranking ngrams by Freq
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount
}

ng2p2 <- function(dcount){
    #aggregate Freq sums
    dcount <- aggregate(dcount$Freq, list(dcount$ngrams), FUN="sum")
    names(dcount) <- c("ngrams", "Freq")
    #ranking ngrams by Freq
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    #splitting them for prediction
    dcount$ngrams <- as.character(dcount$ngrams)
    sn <- strsplit(dcount$ngrams, " ")
    dcount$w1 <- as.character("")
    dcount$w2 <- as.character("")
    dcount$w3 <- sapply(sn, function(sn){sn[1]})
    dcount$w4 <- sapply(sn, function(sn){sn[2]})
    dcount
}

ng3p1 <- function(data) {
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
    ifelse(length(grep("fuck|shit|damn|crap|bitch", allw, ignore.case=T))>0, 
           allw <- allw[-grep("fuck|shit|damn|crap|bitch", allw, ignore.case=T)], 
           allw <- allw)
    ##dealing with dashes
    dashi <- grep("-", allw)
    gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", allw)
    dmatch <- match(gdashi, dashi)
    bdashi <- dashi[-dmatch]
    ifelse(length(bdashi)>0, 
           allw <- allw[-bdashi], 
           allw <- allw)
    ##getting rid of hashtags
    ifelse(length(grep("#[A-Za-z0-9]+", allw))>0, 
           allw <- allw[-grep("#[A-Za-z0-9]+", allw)], 
           allw <- allw)
    #make ngrams
    ngrams <- make.ngrams(allw, 3)
    #compiling counts of ngrams
    dcount <- data.frame(table(ngrams))
    ##getting rid of puncuation
    dcount <- dcount[-grep("<>", dcount$ngrams),]
    #get rid of singletons
    dcount <- dcount[dcount$Freq>3,]
    #ranking ngrams by Freq
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount
}

ng3p2 <- function(dcount){
    #aggregate Freq sums
    dcount <- aggregate(dcount$Freq, list(dcount$ngrams), FUN="sum")
    names(dcount) <- c("ngrams", "Freq")
    #ranking ngrams by Freq
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    #splitting them for prediction
    dcount$ngrams <- as.character(dcount$ngrams)
    sn <- strsplit(dcount$ngrams, " ")
    dcount$w1 <- as.character("")
    dcount$w2 <- sapply(sn, function(sn){sn[1]})
    dcount$w3 <- sapply(sn, function(sn){sn[2]})
    dcount$w4 <- sapply(sn, function(sn){sn[3]})
    dcount
}

ng4p1 <- function(data) {
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
    ifelse(length(grep("fuck|shit|damn|crap|bitch", allw, ignore.case=T))>0, 
           allw <- allw[-grep("fuck|shit|damn|crap|bitch", allw, ignore.case=T)], 
           allw <- allw)
    ##dealing with dashes
    dashi <- grep("-", allw)
    gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", allw)
    dmatch <- match(gdashi, dashi)
    bdashi <- dashi[-dmatch]
    ifelse(length(bdashi)>0, 
           allw <- allw[-bdashi], 
           allw <- allw)
    ##getting rid of hashtags
    ifelse(length(grep("#[A-Za-z0-9]+", allw))>0, 
           allw <- allw[-grep("#[A-Za-z0-9]+", allw)], 
           allw <- allw)
    #make ngrams
    ngrams <- make.ngrams(allw, 4)
    #compiling counts of ngrams
    dcount <- data.frame(table(ngrams))
    ##getting rid of puncuation
    dcount <- dcount[-grep("<>", dcount$ngrams),]
    #get rid of singletons
    dcount <- dcount[dcount$Freq>3,]
    #ranking ngrams by Freq
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount
}

ng4p2a <- function(dcount){
    #aggregate Freq sums
    dcount <- aggregate(dcount$Freq, list(dcount$ngrams), FUN="sum")
    names(dcount) <- c("ngrams", "Freq")
    #ranking ngrams by Freq
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount
}

ng4p2 <- function(dcount){
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
    dcount
}

ngF <- function(ng2, ng3, ng4){
    ngF <- rbind(ng4, ng3, ng2)
    #get rid of singletons
    #ngF <- ngF[ngF$Freq>1,]
    #create a score column?
    ngF
}

set.seed(3030)
b1 <- blogs[1:1000]

ng2b1 <- ng2(b1)
ng3b1 <- ng3(b1)
ng4b1 <- ng4(b1)
ngFb1 <- ngF(ng2b1, ng3b1, ng4b1)

> system.time(ng2blogTr <- ng2(blogTr))
# user  system elapsed 
#326.23    0.76  328.48
write.csv(ng2blogTr, "./pdata/ng2blogTr.csv")

> system.time(ng3blogTr <- ng3(blogTr))
#Error: cannot allocate vector of size 80.4 Mb
# In addition: Warning messages: ...
blog

#NG2
system.time(ng2p1blogTr <- ng2p1(blogTr))
#user  system elapsed 
#293.59    0.85  295.78
system.time(ng2p1twitTr <- ng2p1(twitTr))
#user  system elapsed 
#241.57    0.89  243.63 
system.time(ng2p1newsTr <- ng2p1(newsTr))
#user  system elapsed 
#288.93    1.48  291.53

ng2Tr <- rbind(ng2p1blogTr, ng2p1twitTr, ng2p1newsTr)
system.time(ng2Tr <- ng2p2(ng2Tr))
#user  system elapsed 
#137.36    0.14  138.31

write.csv(ng2Tr, "./pdata/ng2Tr.csv")

system.time(ng2blogTr <- ng2p2(ng2p1blogTr))
system.time(ng2twitTr <- ng2p2(ng2p1twitTr))
system.time(ng2newsTr <- ng2p2(ng2p1newsTr))

write.csv(ng2blogTr, "./pdata/ng2blogTr.csv")
write.csv(ng2twitTr, "./pdata/ng2twitTr.csv")
write.csv(ng2newsTr, "./pdata/ng2newsTr.csv")

#NG3
blogTrA <- blogTr[1:300000]
blogTrB <- blogTr[300001:600000]
blogTrC <- blogTr[600001:length(blogTr)]

system.time(ng3p1blogTrA <- ng3p1(blogTrA))
system.time(ng3p1blogTrB <- ng3p1(blogTrB))
system.time(ng3p1blogTrC <- ng3p1(blogTrC))

twitTrA <- twitTr[1:500000]
twitTrB <- twitTr[500001:1000000]
twitTrC <- twitTr[1000001:length(twitTr)]

system.time(ng3p1twitTrA <- ng3p1(twitTrA))
system.time(ng3p1twitTrB <- ng3p1(twitTrB))
system.time(ng3p1twitTrC <- ng3p1(twitTrC))

newsTrA <- newsTr[1:250000]
newsTrB <- newsTr[250001:500000]
newsTrC <- newsTr[500001:length(newsTr)]

system.time(ng3p1newsTrA <- ng3p1(newsTrA))
system.time(ng3p1newsTrB <- ng3p1(newsTrB))
system.time(ng3p1newsTrC <- ng3p1(newsTrC))

#average processing
#user  system elapsed 
#255.72    1.29  258.35 

ng3Tr <- rbind(ng3p1blogTrA, ng3p1blogTrB, ng3p1blogTrC,
               ng3p1twitTrA, ng3p1twitTrB, ng3p1twitTrC,
               ng3p1newsTrA, ng3p1newsTrB, ng3p1newsTrC)
#rbind took a long time (10 mins or so) should've used system.time
system.time(ng3Tr <- ng3p2(ng3Tr))
#user  system elapsed 
#61.84    8.79  342.28 
write.csv(ng3Tr, "./pdata/ng3Tr.csv")

#NG4
blogTrA <- blogTr[1:300000]
blogTrB <- blogTr[300001:600000]
blogTrC <- blogTr[600001:length(blogTr)]

system.time(ng4p1blogTrA <- ng4p1(blogTrA))
#user  system elapsed 
#602.20    2.79  608.89
system.time(ng4p1blogTrB <- ng4p1(blogTrB))
#about the same
system.time(ng4p1blogTrC <- ng4p1(blogTrC))
#shorter

system.time(ng4blogTrComp <- rbind(ng4p1blogTrA, ng4p1blogTrB, ng4p1blogTrC))
system.time(ng4p2ablogTr <- ng4p2a(ng4blogTrComp))
#> dim(ng4p2ablogTr)
#[1] 193215      2
system.time(write.csv(ng4p2ablogTr, "./pdata/ng4p2ablogTr.csv"))

twitTrA <- twitTr[1:500000]
twitTrB <- twitTr[500001:1000000]
twitTrC <- twitTr[1000001:length(twitTr)]

system.time(ng4p1twitTrA <- ng4p1(twitTrA))
#user  system elapsed 
#265.97    1.34  268.78
system.time(ng4p1twitTrB <- ng4p1(twitTrB))
#about the same
system.time(ng4p1twitTrC <- ng4p1(twitTrC))
#user  system elapsed 
# 425.38    1.69  428.66

system.time(ng4twitTrComp <- rbind(ng4p1twitTrA, ng4p1twitTrB, ng4p1twitTrC))
system.time(ng4p2atwitTr <- ng4p2a(ng4twitTrComp))
system.time(write.csv(ng4p2atwitTr, "./pdata/ng4p2atwitTr.csv"))

newsTrA <- newsTr[1:250000]
newsTrB <- newsTr[250001:500000]
newsTrC <- newsTr[500001:length(newsTr)]

system.time(ng4p1newsTrA <- ng4p1(newsTrA))
system.time(ng4p1newsTrB <- ng4p1(newsTrB))
system.time(ng4p1newsTrC <- ng4p1(newsTrC))
#user  system elapsed #on average
#368.93    1.67  371.76 

system.time(ng4newsTrComp <- rbind(ng4p1newsTrA, ng4p1newsTrB, ng4p1newsTrC))
system.time(ng4p2anewsTr <- ng4p2a(ng4newsTrComp))
system.time(write.csv(ng4p2anewsTr, "./pdata/ng4p2anewsTr.csv"))

ng4p2ablogTr <- read.csv("./pdata/ng4p2ablogTr.csv")
ng4p2atwitTr <- read.csv("./pdata/ng4p2atwitTr.csv")
ng4p2anewsTr <- read.csv("./pdata/ng4p2anewsTr.csv")
system.time(ng4Tr <- rbind(ng4p2ablogTr, ng4p2atwitTr, ng4p2anewsTr))
system.time(ng4Tr <- ng4p2(ng4Tr))
write.csv(ng4Tr, "./pdata/ng4Tr.csv")

##BUILD THE PREDS DATA
ng2Tr <- read.csv("./pdata/ng2Tr.csv")
ng3Tr <- read.csv("./pdata/ng3Tr.csv")
ng4Tr <- read.csv("./pdata/ng4Tr.csv")

ng2Tr <- ng2Tr[,2:7]
ng3Tr <- ng3Tr[,2:7]

predsfull <- rbind(ng4Tr, ng3Tr, ng2Tr)
write.csv(predsfull, "./pdata/predsfull.csv")

##PREDS
simpleFpreds <- function(word1, word2, word3){
    preds <- predsfull
    if(length(grep(word3, preds$w3))>0){
        preds <- preds[grep(word3, preds$w3), ]
    }else{preds <- preds}
    if(length(grep(word2, preds$w2))>0){
        preds <- preds[grep(word2, preds$w2), ]
    }else{preds <- preds}
    if(length(grep(word1, preds$w1))>0){
        preds <- preds[grep(word1, preds$w1), ]
    }else{preds <- preds}
    #print(preds$w4[1:3]) 
    View(preds)
}