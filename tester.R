testcon <- file("testSet.txt", "rb")
testSet <- readLines(testcon)
close(testcon)

testconNB <- file("testSetNB.txt", "rb")
testSetNB <- readLines(testconNB)
close(testconNB)


#to concatenate test set
testSet <- c(blogTe, twitTe, newsTe)
testcon<-file("testSet.txt")
writeLines(testSet, testcon)
close(testcon)

testSetNB <- c(blogTe, newsTe)
testconNB <- file("testSetNB.txt")
writeLines(testSetNB, testconNB)
close(testconNB)

round(runif(1, 1, length(testSet)), 0)


##
contestants <- function(testSet){
    pick <- round(runif(1, 1, length(testSet)), 0)
    winner <- gsub("[^A-Za-z0-9'# -]", "", testSet[pick]) #what we're keeping
    words <- unlist(strsplit(winner, "[ ]")) #actually make the split
    start <- round(runif(1, 1, length(words)-3), 0)
    test <- c(words[start], words[start+1], words[start+2])
    print(test)
    print(winner)
}

tester <- function(testSet){
    pick <- round(runif(1, 1, length(testSet)), 0)
    winner <- gsub("[^A-Za-z0-9'# -]", "", testSet[pick]) #what we're keeping
    words <- unlist(strsplit(winner, "[ ]")) #actually make the split
    start <- round(runif(1, 1, length(words)-3), 0)
    print(c(words[start], words[start+1], words[start+2], 
            predictor(words[start], words[start+1], words[start+2])))
    print(winner)
}

bigTest <- function(testSet){
    pred <- character(length=length(testSet))
    answer <- character(length=length(testSet))
    score <- logical(length=length(testSet))
    for(i in 1:length(testSet)){
        words <- gsub("[^A-Za-z0-9'# -]", "", testSet[i]) #what we're keeping
        words <- unlist(strsplit(words, "[ ]")) #actually make the split
        start <- round(runif(1, 1, ifelse(length(words)>3, length(words), 4)-3), 0)
        pred[i] <- predictor(words[start], words[start+1], words[start+2])
        answer[i] <- words[start+3]
        score[i] <- answer[i] %in% pred[i]
    }
    scorecard <- cbind(answer, pred, score)
    View(scorecard)
    print(table(score)["TRUE"]/length(score))
}
system.time(bigTest(testSet[round(runif(10, 1, length(testSet)), 0)]))

learnTest <- function(testSet){
    pred <- character(length=length(testSet))
    answer <- character(length=length(testSet))
    sentence <- character(length=length(testSet))
    ngram <- character(length=length(testSet))
    score <- logical(length=length(testSet))
    for(i in 1:length(testSet)){
        words <- gsub("[^A-Za-z0-9'# -]", "", testSet[i]) #what we're keeping
        words <- unlist(strsplit(words, "[ ]")) #actually make the split
        start <- round(runif(1, 1, ifelse(length(words)>3, length(words), 4)-3), 0)
        output <- predictor(words[start], words[start+1], words[start+2])
        pred[i] <- output[2]
        ngram[i] <- output[1]
        sentence[i] <- paste(words[start], words[start+1], words[start+2], words[start+3])
        answer[i] <- words[start+3]
        score[i] <- answer[i] %in% pred[i]
    }
    scorecard <- cbind(sentence, ngram, pred, answer, score)
    View(scorecard)
    print(table(score)["TRUE"]/length(score))
}
system.time(learnTest(testSet[round(runif(10, 1, length(testSet)), 0)]))

#with predsfull and predsSearchW3Eq
system.time(learnTest(testSet[round(runif(500, 1, length(testSet)), 0)]))
#TRUE 
#0.162 
#user  system elapsed 
#881.97   22.64  910.25

#with predsfull and predsSearchW3Eq
system.time(learnTest(testSet[round(runif(500, 1, length(testSet)), 0)]))
#TRUE 
#0.156 
#user  system elapsed 
#879.59   24.32  909.76

# can we solve this 2-gram problem? with the old good2g?  maybe.
> tester(blogTe)
[1] "Year"
[1] "a"     "local" "New"  
[1] "The way they won the car was that a local New York disc jockey called them up at random"

learnTest <- function(testSet){
    pred <- character(length=length(testSet))
    answer <- character(length=length(testSet))
    sentence <- character(length=length(testSet))
    ngram <- character(length=length(testSet))
    score <- logical(length=length(testSet))
    for(i in 1:length(testSet)){
        words <- gsub("[^A-Za-z0-9'# -]", "", testSet[i]) #what we're keeping
        words <- unlist(strsplit(words, "[ ]")) #actually make the split
        pick <- round(runif(1, ifelse(length(words)>3, 4, length(words)), 
                             length(words)), 0)
        pwords <- as.character(c(words[pick-3], words[pick-2], words[pick-1]))
        output <- predictor(pwords)
        pred[i] <- output[2]
        ngram[i] <- output[1]
        sentence[i] <- paste(words[pick-3], words[pick-2], words[pick-1], words[pick])
        answer[i] <- words[pick]
        score[i] <- answer[i] %in% pred[i]
    }
    scorecard <- cbind(sentence, ngram, pred, answer, score)
    View(scorecard)
    print(table(score)["TRUE"]/length(score))
}
