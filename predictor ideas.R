setwd("C:/Users/Seth/Documents/Coursera/Capstone")
library(stylo)

predsfull <- read.csv("./pdata/predsfull.csv", stringsAsFactors=F)
preds20 <- predsfull[predsfull$Freq>19,]
preds10 <- predsfull[predsfull$Freq>9, ]
predsfullperp <- read.csv("./pdata/predsfullperp.csv", stringsAsFactors=F)

plot(predsfull$X, predsfull$Freq, type="l", ylim=c(0,50))

# to work off the continued typing
preds <- preds[grep(^word4, preds$w4), ] #the ^ may fuck things up, but we might be good without it

# need to add a better "score", maybe perplexity
simpleGpreds <- function(word1, word2, word3){
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

simpleEpreds <- function(word1, word2, word3){
    preds <- predsfull
    if(length(dim(preds[preds$w3==word3, ])[1])>0){ ##works, but slower than grep by a lot
        preds <- preds[preds$w3==word3, ]  ############can we make grep match exactly (fixed=T didn't work)
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

predsNoAlt <- function(word1, word2, word3){
    preds <- data  ##give preds a tag of n
    #word3, 2grams
    if(length(grep(word3, preds$w3))>0){
        preds <- preds[grep(word3, preds$w3), ]
    }else{preds <- preds[-preds$tag==2, ]}
    ##maybe do a backoff for w3
    #word2, 3grams
    if(length(grep(word2, preds$w2))>0){
        preds <- preds[grep(word2, preds$w2), ]
    }else{preds <- preds[-preds$tag==3, ]}
    #word1, 4grams
    if(length(grep(word1, preds$w1))>0){
        preds <- preds[grep(word1, preds$w1), ]
    }else{preds <- preds}
    #print(preds$w4[1:3]) 
    View(preds)
}

predsSearchW3Grep <- function(word1, word2, word3){
    preds <- predsfull 
    #word3, 2grams
    if(length(grep(word3, preds$w3))>0){
        preds <- preds[grep(word3, preds$w3), ]
    }else{preds <- preds[!preds$tag==2, ]}
    ##maybe do a backoff for w3
    #word2, 3grams
    if(length(grep(word2, preds$w2))>0){
        preds <- preds[grep(word2, preds$w2), ]
    }else{preds <- preds[!preds$tag==3, ]}
    #word1, 4grams
    if(length(grep(word1, preds$w1))>0){
        preds <- preds[grep(word1, preds$w1), ]
    }else{preds <- preds[!preds$tag==4, ]}
    #search w3
    if(length(preds$w4)>0){View(preds)
    }else{if(length(grep(word2, predsfull$w3))>0){
        preds <- predsfull[grep(word2, predsfull$w3), ]; View(preds)
    }else{preds <- predsfull[grep(word1, predsfull$w3), ]; View(preds)}
    }
    #print(preds$w4[1:3]) 
}

predsSearchW3Eq <- function(word1, word2, word3){
    preds <- preds10
    #word3, 2grams
    if(length(grep(word3, preds$w3))>0){
        preds <- preds[preds$w3==word3, ]
    }else{preds <- preds[!preds$tag==2, ]}
    ##maybe do a backoff for w3
    #word2, 3grams
    if(length(grep(word2, preds$w2))>0){
        preds <- preds[preds$w2==word2, ]
    }else{preds <- preds[!preds$tag==3, ]}
    #word1, 4grams
    if(length(grep(word1, preds$w1))>0){
        preds <- preds[preds$w1==word1, ]
    }else{preds <- preds[!preds$tag==4, ]}
    #remove erroneous NAs
    preds <- preds[complete.cases(preds$ngrams), ]
    #search w3
    if(length(preds$w4)>0){print(preds$w4[1])
    }else{if(length(grep(word2, predsfull$w3))>0){
        preds <- predsfull[predsfull$w3==word2, ]
        preds <- preds[complete.cases(preds$ngrams), ]
        print(preds$w4[1])
    }else{preds <- predsfull[predsfull$w3==word1, ]
          preds <- preds[complete.cases(preds$ngrams), ]
          print(preds$w4[1])}
    }
    View(preds)
}

preds[complete.cases(preds$ngrams), ]

length(preds2$ngrams[complete.cases(preds2$ngrams)])

#====predictor

#predsSearchW3Eq for bigTest
predictor <- function(word1, word2, word3){
    preds <- preds10
    #word3, 2grams
    if(length(grep(word3, preds$w3))>0){
        preds <- preds[preds$w3==word3, ]
    }else{preds <- preds[!preds$tag==2, ]}
    ##maybe do a backoff for w3
    #word2, 3grams
    if(length(grep(word2, preds$w2))>0){
        preds <- preds[preds$w2==word2, ]
    }else{preds <- preds[!preds$tag==3, ]}
    #word1, 4grams
    if(length(grep(word1, preds$w1))>0){
        preds <- preds[preds$w1==word1, ]
    }else{preds <- preds[!preds$tag==4, ]}
    #remove erroneous NAs
    preds <- preds[complete.cases(preds$ngrams), ]
    #search w3
    if(length(preds$w4)>0){preds$w4[1]
    }else{if(length(grep(word2, predsfull$w3))>0){
        preds <- predsfull[predsfull$w3==word2, ]
        preds <- preds[complete.cases(preds$ngrams), ]
        preds$w4[1]
    }else{preds <- predsfull[predsfull$w3==word1, ]
          preds <- preds[complete.cases(preds$ngrams), ]
          preds$w4[1]}
    }
}

#predsSearchW3Eq for learnTest
predictor <- function(word1, word2, word3){
    #predsfull <- predsfullperp
    preds <- predsfull
    #word3, 2grams
    if(length(grep(word3, preds$w3))>0){
        preds <- preds[preds$w3==word3, ]
    }else{preds <- preds[!preds$tag==2, ]}
    ##maybe do a backoff for w3
    #word2, 3grams
    if(length(grep(word2, preds$w2))>0){
        preds <- preds[preds$w2==word2, ]
    }else{preds <- preds[!preds$tag==3, ]}
    #word1, 4grams
    if(length(grep(word1, preds$w1))>0){
        preds <- preds[preds$w1==word1, ]
    }else{preds <- preds[!preds$tag==4, ]}
    #remove erroneous NAs
    preds <- preds[complete.cases(preds$ngrams), ]
    output <- character(length=2)
    #search w3
    if(length(preds$w4)>0){output[1]<- preds$ngram[1]; output[2]<- preds$w4[1]
    }else{if(length(grep(word2, predsfull$w3))>0){
        preds <- predsfull[predsfull$w3==word2, ]
        preds <- preds[complete.cases(preds$ngrams), ]
        preds$ngram[1]; output[2]<- preds$w4[1]
    }else{preds <- predsfull[predsfull$w3==word1, ]
          preds <- preds[complete.cases(preds$ngrams), ]
          preds$ngram[1]; output[2]<- preds$w4[1]}
    }
    output
}

#predsSearchW3Eq for learnTest with is.na protection for pick
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
    output <- character(length=2)
    #search w3
    if(length(preds$w4)>0){output[1]<- preds$ngram[1]; output[2]<- preds$w4[1]
    }else{
        if(is.na(pwords[2])){predsfull$ngram[1]; output[2]<- predsfull$w4[1]
        }else if(length(grep(pwords[2], predsfull$w3))>0){
            preds <- predsfull[predsfull$w3==pwords[2], ]
            preds <- preds[complete.cases(preds$ngrams), ]
            preds$ngram[1]; output[2]<- preds$w4[1]
        }else if(is.na(pwords[1])){predsfull$ngram[1]; output[2]<- predsfull$w4[1]
        }else{preds <- predsfull[predsfull$w3==pwords[1], ]
            preds <- preds[complete.cases(preds$ngrams), ]
            preds$ngram[1]; output[2]<- preds$w4[1]}
    }
    output
}