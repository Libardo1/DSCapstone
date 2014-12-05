## FIRST TRY AT FULL THING
pred1 <- function(sentence) {
    preds <- datafull
    sentence <- gsub("[^A-Za-z0-9'# -]", "", sentence) #what we're keeping
    words <- unlist(strsplit(sentence, "[ ]")) #actually make the split
    word2 <- words[length(words)]
    if(length(grep(word2, preds$w2))>0){
        preds <- preds[grep(word2, preds$w2), ]
    }else{word2 <- words[length(words)-1]}
    if(length(grep(word2, preds$w2))>0){
        preds <- preds[grep(word2, preds$w2), ]
    }else{word2 <- words[length(words)-2]}
    if(length(grep(word2, preds$w2))>0){
        preds <- preds[grep(word2, preds$w2), ]
    }else{preds <- gcount}
    word1 <- words[max(grep(word2, words))-1]
    if(length(grep(word1, preds$w1))>0){
        preds <- preds[grep(word1, preds$w1), ]
    }else{word1 <- words[max(grep(word2, words))-2]}
    if(length(grep(word1, preds$w1))>0){
        preds <- preds[grep(word1, preds$w1), ]
    }else{word1 <- words[max(grep(word2, words))-3]}
    if(length(grep(word1, preds$w1))>0){
        preds <- preds[grep(word1, preds$w1), ]
    }else{preds <- preds}
    print(as.character(preds$w3[1:3]))
}

===

## THE PREDICTION
predictor <- function(word1, word2){
    if(length(grep(word1, gcount$w1))>0){
        preds <- gcount[grep(word1, gcount$w1), ]
    }else{preds <- gcount}
    if(length(grep(word2, preds$w2))>0){
        preds <- preds[grep(word2, preds$w2), ]
    }else{preds <- preds}
    print(preds$w3[1:3]) 
}

predopts <- function(word1, word2, gcount){
    if(length(grep(word1, gcount$w1))>0){
        preds <- gcount[grep(word1, gcount$w1), ]
    }else{preds <- gcount}
    if(length(grep(word2, preds$w2))>0){
        preds <- preds[grep(word2, preds$w2), ]
    }else{preds <- preds}
    as.character(preds$w3)
}

#===

##THE SETUP
words <- c("a", "b", "c", "d", "e")

set.seed(3535)
n2 <- sample(words, 10000, replace=T)
n3 <- sample(words, 10000, replace=T)
n4 <- sample(words, 10000, replace=T)

ngrams <- paste(n2, n3, n4)

## THE PROCESSING (once you've got the ngrams)
ngcount <- data.frame(table(ngrams))
ngcount <- gcount[order(ngcount$Freq, decreasing=T),]
ngcount$ngrams <- as.character(ngcount$ngrams)

sn <- strsplit(ngcount$ngrams, " ")

for(i in 1:dim(ngcount)[1]){ngcount$w1[i] <- sn[[i]][1]}
for(i in 1:dim(ngcount)[1]){ngcount$w2[i] <- sn[[i]][2]}
for(i in 1:dim(ngcount)[1]){ngcount$w3[i] <- sn[[i]][3]}

## THE PREDICTION
predictor <- function(word1, word2){
    if(length(grep(word1, gcount$w1))>0){
        preds <- gcount[grep(word1, gcount$w1), ]
    }else{preds <- gcount}
    if(length(grep(word2, preds$w2))>0){
        preds <- preds[grep(word2, preds$w2), ]
    }else{preds <- preds}
    print(preds$w3[1:3]) 
}

#=====
dim(gcount)[1]

lapply(sn, [1])

sn <- unlist(strsplit(gcount$ngrams, " "))
snd <- as.data.frame(sn)
gcount$w1 <- as.character(snd[1,])
gcount$w2 <- snd[2,]
gcount$w3 <- snd[3.]

gcount[grep("a", gcount$w1),]

gw1 <- grep("a", gcount$w1)
if(length(gw1)>0){preds <- gcount[gw1, ]
}else{preds <- gcount}
gw2 <- grep("b", preds$w2)
if(length(gw2)>0){preds <- preds[gw2, ]
}else{preds <- preds}


if(length(grep("a", gcount$w1))>0){preds <- gcount[grep("a", gcount$w1), ]
    }else{preds <- gcount}
if(length(grep("b", preds$w2))>0){preds <- preds[grep("b", preds$w2), ]
    }else{preds <- preds}
print(preds$w3[1])

max(preds$Freq)
print(preds$w3[preds$Freq==max(preds$Freq),]) # could work, but messy


### predicting by hand WORKS!!!
dcount$ngrams <- as.character(dcount$ngrams)
sn <- strsplit(dcount$ngrams, " ")

    #this part takes awhile, even with a twitsmall
for(i in 1:dim(dcount)[1]){dcount$w1[i] <- sn[[i]][1]}
for(i in 1:dim(dcount)[1]){dcount$w2[i] <- sn[[i]][2]}
for(i in 1:dim(dcount)[1]){dcount$w3[i] <- sn[[i]][3]}

if(length(grep("a", gcount$w1))>0){
    preds <- gcount[grep("a", gcount$w1), ]
    }else{preds <- gcount}
if(length(grep("b", preds$w2))>0){
    preds <- preds[grep("b", preds$w2), ]
    }else{preds <- preds}
print(preds$w3[1])

