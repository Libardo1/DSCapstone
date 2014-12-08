library(stylo)

predsfull <- read.csv("./pdata/predsfull.csv", stringsAsFactors=F)

bigPicker <- function(words){
    words <- gsub("[^A-Za-z0-9'# -]", "", words) #what we're keeping
    words <- unlist(strsplit(words, "[ ]")) #actually make the split
    pwords <- as.character(c(words[length(words)-2], words[length(words)-2], words[length(words)]))
    predictor <- function(pwords){
        preds <- predsfull
        #word3, 2grams
        if(length(grep(pwords[3], preds$w3))>0){
            preds <- preds[preds$w3==pwords[3], ]
        }else{preds <- preds[!preds$tag==2, ]}
        ##maybe do a backoff for w3
        #word2, 3grams
        if(is.na(pwords[2])){preds <- preds[!preds$tag==3, ]
        }else if(length(grep(pwords[2], preds$w2))>0){
            preds <- preds[preds$w2==pwords[2], ]
        }else{preds <- preds[!preds$tag==3, ]}
        #word1, 4grams
        if(is.na(pwords[1])){preds <- preds[!preds$tag==4, ]
        }else if(length(grep(pwords[1], preds$w1))>0){
            preds <- preds[preds$w1==pwords[1], ]
        }else{preds <- preds[!preds$tag==4, ]}
        #remove erroneous NAs
        preds <- preds[complete.cases(preds$ngrams), ]
        output <- character(length=1)
        #search w3
        if(length(preds$w4)>0){output <- preds$w4[1]
        }else{
            if(is.na(pwords[2])){predsfull$ngram[1]; output <- predsfull$w4[1]
            }else if(length(grep(pwords[2], predsfull$w3))>0){
                preds <- predsfull[predsfull$w3==pwords[2], ]
                preds <- preds[complete.cases(preds$ngrams), ]
                output <- preds$w4[1]
            }else if(is.na(pwords[1])){predsfull$ngram[1]; output[2]<- predsfull$w4[1]
            }else{preds <- predsfull[predsfull$w3==pwords[1], ]
                  preds <- preds[complete.cases(preds$ngrams), ]
                  output <- preds$w4[1]}
        }
        output
    }
    answer <- predictor(pwords)
    answer
}






#==============

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
