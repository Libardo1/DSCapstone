setwd("C:/Users/Seth/Documents/Coursera/Capstone")
library(stylo)
#library(tm)
#library(openNLP)

## BLOGS
blogcon <- file("./Coursera-Swiftkey/final/en_US/en_US.blogs.txt", "rb")
blogs <- readLines(blogcon)
close(blogcon)

set.seed(3030)
blogTrain <- round(runif(round(length(blogs)*0.75, 0), 1, length(blogs)), 0)
blogTr <- blogs[blogTrain]
blogTe <- blogs[-blogTrain]

## TWITTER
twitcon <- file("./Coursera-Swiftkey/final/en_US/en_US.twitter.txt", "rb")
twitter <- readLines(twitcon)
close(twitcon)

set.seed(3030)
twitTrain <- round(runif(round(length(twitter)*0.75, 0), 1, length(twitter)), 0)
twitTr <- twitter[twitTrain]
twitTe <- twitter[-twitTrain]

## NEWS
newscon <- file("./Coursera-Swiftkey/final/en_US/en_US.news.txt", "rb")
news <- readLines(newscon)
close(newscon)

set.seed(3030)
newsTrain <- round(runif(round(length(news)*0.75, 0), 1, length(news)), 0)
newsTr <- news[newsTrain]
newsTe <- news[-newsTrain]

##
TGdatafull <- read.csv("./pdata/TGdatafull.csv")

TGdatafullnb <- read.csv("./pdata/TGdatafullnb.csv")
TGdatafulltwit <- read.csv("./pdata/TGdatafulltwit.csv")
TGdatafullnews <- read.csv("./pdata/datafullnews.csv")
TGdatafullblog <- read.csv("./pdata/datafullblog.csv")

"./Coursera-Swiftkey/final/en_US/"
"C:/Users/Seth/Documents/Coursera/Capstone/Coursera-Swiftkey/final/en_US/"

## MANUAL BACKOFF PREDICTOR
predtableE <- function(sentence) {
    preds <- datafull
    sentence <- gsub("[^A-Za-z0-9'# -]", "", sentence) #what we're keeping
    words <- unlist(strsplit(sentence, "[ ]")) #actually make the split
    word2 <- words[length(words)]
    if(dim(na.omit(preds[preds$w2==word2, ]))[1]>0){
        preds <- preds[preds$w2==word2, ]
    }else{word2 <- words[length(words)-1]}
    if(dim(na.omit(preds[preds$w2==word2, ]))[1]>0){
        preds <- preds[preds$w2==word2, ]
    }else{word2 <- words[length(words)-2]}
    if(dim(na.omit(preds[preds$w2==word2, ]))[1]>0){
        preds <- preds[preds$w2==word2, ]
    }else{preds <- preds; word2 <- words[length(words)]}
    word1 <- words[max(grep(word2, words))-1]
    if(dim(na.omit(preds[preds$w1==word1, ]))[1]>0){
        preds <- preds[preds$w1==word1, ]
    }else{word1 <- words[max(grep(word2, words))-2]}
    if(dim(na.omit(preds[preds$w1==word1, ]))[1]>0){
        preds <- preds[preds$w1==word1, ]
    }else{word1 <- words[max(grep(word2, words))-3]}
    if(dim(na.omit(preds[preds$w1==word1, ]))[1]>0){
        preds <- preds[preds$w1==word1, ]
    }else{preds <- preds}
    View(na.omit(preds))
}

## THE CONSTRUCTOR
### works, but have to do it in chunks of 500k tweets or 200k articles/blogs
build1 <- function(data) {
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
    allw <- allw[-grep("fuck|shit|damn|crap|bitch", allw, ignore.case=T)]
    ngrams <- make.ngrams(allw, 3)
    #compiling counts of ngrams
    dcount <- data.frame(table(ngrams))
    #getting rid of stupid ngrams
    ##dealing with dashes
    dashi <- grep("-", dcount$ngrams)
    gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$ngrams)
    dmatch <- match(gdashi, dashi)
    bdashi <- dashi[-dmatch]
    dcount <- dcount[-bdashi, ]
    ##getting rid of hashtags
    dcount <- dcount[-grep("#[A-Za-z0-9]+", dcount$ngrams), ]
    #get rid of singletons
    dcount <- dcount[dcount$Freq>1,]
    dcount
}

build2 <- function(dcount){
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
    ##getting rid of random unpredictable puncuation
    dcount <- dcount[-grep("<>", dcount$w2),]
    dcount <- dcount[-grep("<>", dcount$w3),]
    dcount
}


## Twitter Binary
twitcon <- file("./Coursera-Swiftkey/final/en_US/en_US.twitter.txt", "rb")
twitter <- readLines(twitcon)
length(twitter)
twitter[167155]
twit10 <- twitter[round(runif(10, 0, length(twitter)+1), 0)]

twit1 <- build1(twitter[1:500000])
twit2 <- build1(twitter[500001:1000000])
twit3 <- build1(twitter[1000001:1500000])
twit4 <- build1(twitter[1500001:2000000])
twit5 <- build1(twitter[2000001:length(twitter)])
#...
rawtwit <- rbind(twit1, twit2, twit3, twit4, twit5)

datafulltwit <- build2(rawtwit)

write.csv(datafulltwit, "datafulltwit.csv")

## Blogs Binary
blogcon <- file("./Coursera-Swiftkey/final/en_US/en_US.blogs.txt", "rb")
blogs <- readLines(blogcon)
length(blogs)
# [1] 899288
blogs[167155]

blog1 <- build1(blogs[1:200000])
blog2 <- build1(blogs[200001:400000])
blog3 <- build1(blogs[400001:600000])
blog4 <- build1(blogs[600001:800000])
blog5 <- build1(blogs[800001:length(blogs)])

rawblog <- rbind(blog1, blog2, blog3, blog4, blog5)

datafullblog <- build2(rawblog)

write.csv(datafullblog, "datafullblog.csv")

## News Binary
newscon <- file("./Coursera-Swiftkey/final/en_US/en_US.news.txt", "rb")
news <- readLines(newscon)
length(news)
# [1] 1010242
news[77259]
# [1] "At the Spice Merchant, a 2-ounce bag of tea leaves capable of producing 1\032 gallons of iced tea costs about $2.50."

news1 <- build1(news[1:200000])
news2 <- build1(news[200001:400000])
news3 <- build1(news[400001:600000])
news4 <- build1(news[600001:800000])
news5 <- build1(news[800001:length(news)])

rawnews <- rbind(news1, news2, news3, news4, news5)

datafullnews <- build2(rawnews)

write.csv(datafullnews, "datafullnews.csv")

## COMBINE NEWS AND BLOG
nb <- rbind(datafullnews, datafullblog)
nb2 <- nb[,c(2,3)]
datafullnb <- build2(nb2)
write.csv(datafullnb, "datafullnb.csv")

## COMBINE EVERYTHING
full <- rbind(datafullnews, datafullblog, datafulltwit)
full2 <- full[,c(2,3)]
datafull <- build2(full2)
write.csv(datafull, "datafull.csv")

## BIG TIME NGRAMMER: NGCOUNT
ngcount <- function(data, number) {
    #subsetting the data we want
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    #cleaning puncuation and capitalization
    word <- gsub("([\\.\\?!])", "<>", word) #replace . ? ! with <>
    word <- gsub("<>", "~<>~", word) #split at <>
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    # collapsing and ngramming
    allone <- paste(word, collapse = '')
    allw <- txt.to.words(allone, splitting.rule = "[ ~]", preserve.case = T)
    ##filter out profanity
    allw <- allw[-grep("fuck|shit|damn|crap|bitch", allw, ignore.case=T)]
    ngrams <- make.ngrams(allw, 3)
    #compiling counts of ngrams
    dcount <- data.frame(table(ngrams))
    #getting rid of stupid ngrams
    ##dealing with dashes
    dashi <- grep("-", dcount$ngrams)
    gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$ngrams)
    dmatch <- match(gdashi, dashi)
    bdashi <- dashi[-dmatch]
    dcount <- dcount[-bdashi, ]
    ##getting rid of hashtags
    dcount <- dcount[-grep("#[A-Za-z0-9]+", dcount$ngrams), ]
    #ranking ngrams by Freq
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    #splitting them for prediction
    dcount$ngrams <- as.character(dcount$ngrams)
    sn <- strsplit(dcount$ngrams, " ")
    dcount$w1 <- sapply(sn, function(sn){sn[1]})
    dcount$w2 <- sapply(sn, function(sn){sn[2]})
    dcount$w3 <- sapply(sn, function(sn){sn[3]})
    ##getting rid of random unpredictable puncuation
    dcount <- dcount[-grep("<>", dcount$w2),]
    dcount <- dcount[-grep("<>", dcount$w3),]
    #get rid of singletons
    dcount <- dcount[dcount$Freq>1,]
    dcount
}
## > twit500 <- ngcount(twitter, 500000)
## took about 7 minutes to process 500,000 twitter entries

## bigtwit1 <- ngcount(twitter, length(twitter))
## started 8:24
## write.csv(bigtwit1, "bigtwit1.csv")

## TOKENIZER
tokenizer <- function(data, number) {
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
    #word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- unlist(strsplit(word, "[ ~]")) #actually make the split
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    dcount <- data.frame(table(word))
    dcount <- dcount[grep(".+", dcount$word), ] #remove blank 
    #should we remove periods? dcount <- dcount[-grep("\\.", dcount$word), ]
        dashi <- grep("-", dcount$word) #all dashes
        gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$word) #good dashes
        dmatch <- match(gdashi, dashi) #indices of dashi that are matched by gdashi
        bdashi <- dashi[-dmatch] #bad dashes
    dcount <- dcount[-bdashi, ] #removes bad dashes
    dcount <- dcount[-grep("fuck|shit|damn|crap|bitch", dcount$word, ignore.case=T), ]
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount
}
twitcounts <- tokenizer(twitter, length(twitter))
twitwords <- twitcounts[grep("[A-Za-z]+", twitcounts$word), ]

## NGRAMMER
ngrammer <- function(data, number) {
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
    word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    allone <- paste(word, collapse = '')
    allw <- txt.to.words(allone, splitting.rule = "[ ~]", preserve.case=T)
    ngrams <- make.ngrams(allw, 3)
    ngrams
    dcount <- data.frame(table(ngrams))
    dashi <- grep("-", dcount$ngrams)
    gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$ngrams)
    dmatch <- match(gdashi, dashi) #indices of dashi that are matched by gdashi
    bdashi <- dashi[-dmatch]
    dcount <- dcount[-bdashi, ]
    dcount <- dcount[-grep("[!\\?#\\.] ?[!\\?#\\.]+", dcount$ngrams), ]
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount
}
twitng <- ngrammer(twitter, 30000)
twitsmall <- ngrammer(twitter, 3000)

## Blogs Binary
blogcon <- file("./Coursera-Swiftkey/final/en_US/en_US.blogs.txt", "rb")
blogs <- readLines(blogcon)
length(blogs)
blogs[167155]

blogcounts <- tokenizer(blogs, length(blogs))
blogwords <- blogcounts[grep("[A-Za-z]+", blogcounts$word), ]

## News Binary
newscon <- file("./Coursera-Swiftkey/final/en_US/en_US.news.txt", "rb")
news <- readLines(newscon)
length(news)
# [1] 1010242
news[77259]
# [1] "At the Spice Merchant, a 2-ounce bag of tea leaves capable of producing 1\032 gallons of iced tea costs about $2.50."

newscounts <- tokenizer(news, length(news))
newswords <- newscounts[grep("[A-Za-z]+", newscounts$word), ]

close(twitcon)
close(blogcon)
close(newscon)

## EXPLORATORY

#top 20 barplot
barplot(twitwords$Freq[1:20]/1000, names.arg=twitwords$word[1:20], axisnames=T, 
        main="20 Most Common Words", las=1, horiz=T,
        xlab="Word Count (in thousands)")

#top 100 barplot
barplot(twitwords$Freq[1:100]/1000, names.arg=c(1:100), axisnames=T, 
        main="100 Most Common Words", 
        xlab= "Rank", ylab="Word Count (in thousands)")

#top 500 barplot
barplot(twitwords$Freq[1:500]/1000, names.arg=c(1:500), axisnames=T, 
        main="500 Most Common Words", 
        xlab= "Rank", ylab="Word Count (in thousands)")

##
total <- sum(dcount$Freq) #total in corpus

sum(dcount$Freq[1:500])

> sum(dcount$Freq[1:157])/total
[1] 0.5005109

> sum(dcount$Freq[1:8800])/total
[1] 0.9002006

sum(dcount$Freq[1:8800])/total

hoss <- datafull[datafull$w1=="horse",]

> dcount <- datafull
> sum(dcount$Freq[dcount$Freq<5])/total
[1] 0.1266074
> sum(dcount$Freq[dcount$Freq<20])/total
[1] 0.2944325
> sum(dcount$Freq[dcount$Freq<50])/total
[1] 0.4416397
> mean(dcount$Freq)
[1] 12.54169
> sd(dcount$Freq)
[1] 100.732
> var(dcount$Freq)
[1] 10146.94

#=====

round(runif(5, 0, 14),0)

round(runif(10, 0, length(blogs)+1), 0)

news10 <- news[round(runif(10, 0, length(news)+1), 0)]
news3 <- news10[1:3]

strsplit(news10, " ")


#take 10 news articles
round(runif(10, 0, length(news)+1), 0)
news10 <- news[round(runif(10, 0, length(news)+1), 0)]

strsplit(news10, " ")

con <- file("en_US.twitter.txt", "r") 
readLines(con, 1) ## Read the first line of text 
readLines(con, 1) ## Read the next line of text 
readLines(con, 5) ## Read in the next 5 lines of text 
close(con) ## It's important to close the connection when you are done

twitcon <- file("C:/Users/Seth/Documents/Coursera/Capstone/Coursera-Swiftkey/final/en_US/en_US.twitter.txt", "r") 
readLines(twitcon, 1) ## Read the first line of text 
close(twitcon) ## It's important to close the connection when you are done

bob <- readLines(twitcon, 4)
sample(bob, size=2)

## News Binary
con <- file("./Coursera-Swiftkey/final/en_US/en_US.news.txt", "rb")
news <- readLines(con)
news[77259]
# [1] "At the Spice Merchant, a 2-ounce bag of tea leaves capable of producing 1\032 gallons of iced tea costs about $2.50."
length(news)
# [1] 1010242

## Twitter Binary
twitcon <- file("./Coursera-Swiftkey/final/en_US/en_US.twitter.txt", "rb")
twitter <- readLines(twitcon)
length(twitter)
twitter[167155]

gsub("([\\])", "", news10, fixed=TRUE)

## BRUTAL BUT WORKS
news3 <- gsub("[^A-Za-z0-9 ]", "", news3)
strsplit(news3, " ")

twit10 <- gsub("[^A-Za-z0-9 ]", "", twit10)
twit10 <- strsplit(twit10, " ")

# THINGS TO CONSIDER INCLUDING
# ? ! ' . #

# Also, maybe some kind of operator to get rid of multiple ... 

gsub("[^..+]", "", twit10)
# [^\.\.+] should work but gives me a stupid error
# maybe not necessary, because who cares

twit10 <- twitter[round(runif(10, 0, length(twitter)+1), 0)]

## PRETTY SOLID, MAYBE STILL A LITTLE BRUTAL BUT I THINK I'M HAPPY
twit10 <- gsub("[^A-Za-z0-9!\\?'#\\. ]", "", twit10)
twit10 <- gsub("([\\.\\?!])", "~\\1~", twit10)
twit10 <- unlist(strsplit(twit10, "[ ~]"))
twit10 <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", twit10, perl=TRUE)
####

twit10 <- unlist(strsplit(twit10, " "))

twit10 <- strsplit(twit10, "[^A-Za-z0-9]")
twit10 <- unlist(strsplit(twit10, split = "[\\.\\?! ]"))
twit10 <- strsplit(twit10, "!")

twit10 <- gsub("([\\.\\?!])", "~\\1~", twit10)
twit10 <- unlist(strsplit(twit10, "[ ~]"))

twit10 <- unlist(strsplit(gsub("[\\.\\?!]", "~\\1~", twit10), " ~"))

# running caps
gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", twit10, perl=TRUE)

## UNIQUE COUNT
count <- data.frame(word = unique(twit10))
count$count <- for(i in 1:length(count$word)){sum(grep(count$word[i], twit10))}

sum(grepl("!", twit10))

count$count <- for(i in 1:length(count$word)){sum(grepl(count$word[i], twit10))}

## just use table
twitcount <- data.frame(table(twit10))

twitcount2 <- twitcount[twitcount$Freq>1,]



## TOKENIZER RAW
twit1000 <- twitter[round(runif(1000, 0, length(twitter)+1), 0)]
twit1000 <- gsub("[^A-Za-z0-9!\\?'#\\. ]", "", twit1000)
twit1000 <- gsub("([\\.\\?!])", "~\\1~", twit1000)
twit1000 <- unlist(strsplit(twit1000, "[ ~]"))
twit1000 <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", twit1000, perl=TRUE)
twitcount <- data.frame(table(twit1000))
twitcount <- twitcount[order(twitcount$Freq, decreasing=T),]
twitcount2 <- twitcount[twitcount$Freq>1,]
####
barplot(twitcount2$Freq[1:100])

####
http://r.789695.n4.nabble.com/Using-FUNCTION-to-create-usable-objects-td4588681.html

#doesn't work
namer <- function(data){
    paste(data, "pow")
}

# how many unique words do you need to cover %50 of the word instances in the language? (do they mean the corpus?)
total <- sum(dcount$Freq) #total in corpus

sum(dcount$Freq[1:500])

> sum(dcount$Freq[1:157])/total
[1] 0.5005109

> sum(dcount$Freq[1:8800])/total
[1] 0.9002006

sum(dcount$Freq[1:8800])/total

hoss <- datafull[datafull$w1=="horse",]

# better dealing with apostrophes in compound words

word <- gsub("([\\.\\?!])", "~\\1~", word)
word <- unlist(strsplit(word, "[ ~]"))

grep("^'", twitwords$word)

apos1 <- twitwords[grep("^'", twitwords$word),]
apos1$rank <- grep("^'", twitwords$word)

word <- gsub("(')", "~\\1", word)

apos3 <- twitwords[grep("'[A-Z]+", twitwords$word), ]
apos3$rank <- grep("'[A-Z]+", twitwords$word)


## GETTING RID OF CAPS
tokeNoCaps <- function(data, number) {
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. ]", "", word)
    word <- gsub("([\\.\\?!])", "~\\1~", word)
    word <- gsub("(')", "~\\1", word)
    word <- unlist(strsplit(word, "[ ~]"))
    word <- gsub("([A-Z]+)", "\\L\\1", word, perl=TRUE)
    dcount <- data.frame(table(word))
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount
}
twitcountsNC <- tokeNoCaps(twitter, length(twitter))
twitwordsNC <- twitcountsNC[grep("[A-Za-z]+", twitcountsNC$word), ]

# reprise with no caps:
# how many unique words do you need to cover %50 of the word instances in the language? (do they mean the corpus?)
totalNC <- sum(twitwordsNC$Freq) #total in corpus

> totalNC
[1] 30343554
> total
[1] 30316111

sum(twitwordsNC$Freq[1:500])

> sum(twitwordsNC$Freq[1:110])/totalNC
[1] 0.5018545

> sum(twitwordsNC$Freq[1:5500])/totalNC
[1] 0.9010754

##PROFANITY FILTER
Prof <- twitwords[grep("fuck|shit|damn|crap|bitch", twitwords$word, ignore.case=T), ]
Prof$rank <- grep("fuck|shit|damn|crap|bitch", twitwords$word, ignore.case=T)

clean <- twitwords[-grep("fuck|shit|damn|crap|bitch", twitwords$word, ignore.case=T), ]

## WORD COUNT BARPLOT
barplot(twitwords$Freq[1:100]/1000, xlab=twitwords$row.names)

#top 100
barplot(twitwords$Freq[1:100]/1000, names.arg=c(1:100), axisnames=T, 
        main="100 Most Common Words", 
        xlab= "Rank", ylab="Word Count (in thousands)")

#top 20
barplot(twitwords$Freq[1:20]/1000, names.arg=twitwords$word[1:20], axisnames=T, 
        main="100 Most Common Words", las=1, horiz=T,
        xlab="Word Count (in thousands)")

# delete blanks whitespace
nothing <- twitcounts[-grep(".+", twitcounts$word), ]
something <- twitcounts[grep(".+", twitcounts$word), ]

# dealing with the dashes
dash <- twitcounts[grep("-", twitcounts$word),]
View(dash)

gooddash <- twitcounts[grep("[A-Za-z0-9]+-[A-Za-z0-9]+", twitcounts$word),]

dashi <- grep("-", twitcounts$word)
gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", twitcounts$word)
dmatch <- match(gdashi, dashi) #indices of dashi that are matched by gdashi
bdashi <- dashi[-dmatch]
twitcounts <- twitcounts[-bdashi, ]

dashi <- grep("-", dcount$word) #all dashes
gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$word) #good dashes
dmatch <- match(gdashi, dashi) #indices of dashi that are matched by gdashi
bdashi <- dashi[-dmatch] #bad dashes
dcount <- dcount[-bdashi, ] #removes bad dashes

#comparing vocab between sources

table(as.character(twitcounts$word[1:20]), as.character(newscounts$word[1:20]))
#not that cool

## GREP IN FUNCTION POST
I'm trying to figure out if there's a way to use grep() within a function where I can search for
the value that's entered into the function.  For example, say we have this (admittedly tiny) example
data frame consisting of trigrams that have been split into three words, and the Frequency
with which they showed up.

Freq <- as.integer(c("95", "87", "65", "55", "43"))
gcount <- as.data.frame(Freq)
gcount$w1 <- c("a", "c", "a", "a", "b")
gcount$w2 <- c("c", "d", "b", "a", "d")
gcount$w3 <- c("d", "b", "a", "d", "c")

I want to be able to do a function like this:

predictor <- function(word1, word2){
    if(length(grep("word1", gcount$w1))>0){
        preds <- gcount[grep("word1", gcount$w1), ]
        }else{preds <- gcount}
    if(length(grep("word2", preds$w2))>0){
        preds <- preds[grep("word2", preds$w2), ]
        }else{preds <- preds}
    print(preds$w3[1]) 
}

Obviously, this doesn't work because grep() only takes a regular expression within quotation marks
so this function (as written) throws an error.  

When I run it like this (manually entering "a" and "b") it works like I want it to.  

if(length(grep("a", gcount$w1))>0){
    preds <- gcount[grep("a", gcount$w1), ]
    }else{preds <- gcount}
if(length(grep("b", preds$w2))>0){
    preds <- preds[grep("b", preds$w2), ]
    }else{preds <- preds}
print(preds$w3[1]) 

Anyone know if there's a trick to be able to use grep like this?
Thanks,
Seth
'

#### NGRAMS
minitwit <- twitcounts[1:500,]

## CLEANER
cleaner <- function(data, number) {
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    
    
    #the split
    word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
    word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- unlist(strsplit(word, "[ ~]")) #actually make the split
    
    dcount <- dcount[-grep("fuck|shit|damn|crap|bitch", dcount$word, ignore.case=T), ]
    
    #the count
    dcount <- data.frame(table(word))
    dcount <- dcount[grep(".+", dcount$word), ] #remove blank 
    #should we remove periods? dcount <- dcount[-grep("\\.", dcount$word), ]
    dashi <- grep("-", dcount$word) #all dashes
    gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$word) #good dashes
    dmatch <- match(gdashi, dashi) #indices of dashi that are matched by gdashi
    bdashi <- dashi[-dmatch] #bad dashes
    dcount <- dcount[-bdashi, ] #removes bad dashes
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount
}

## SPLITTER
splitter <- function(data, number) {
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
    word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- unlist(strsplit(word, "[ ~]")) #actually make the split
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    word
}
twitsplit <- splitter(twitter, 500)
    
    dcount <- data.frame(table(word))
    dcount <- dcount[grep(".+", dcount$word), ] #remove blank 
    #should we remove periods? dcount <- dcount[-grep("\\.", dcount$word), ]
    dashi <- grep("-", dcount$word) #all dashes
    gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$word) #good dashes
    dmatch <- match(gdashi, dashi) #indices of dashi that are matched by gdashi
    bdashi <- dashi[-dmatch] #bad dashes
    dcount <- dcount[-bdashi, ] #removes bad dashes
    dcount <- dcount[-grep("fuck|shit|damn|crap|bitch", dcount$word, ignore.case=T), ]
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount
}

#using make.ngrams
> twit10[1]
[1] "I'm going to be real guys. I don't know what YOLO means."
> txt.to.words(twit10[1])
[1] "i"     "m"     "going" "to"    "be"    "real"  "guys"  "i"     "don"   "t"     "know"  "what"  "yolo"  "means"
> twttxt <- txt.to.words(twit10[1])
> twttxt
[1] "i"     "m"     "going" "to"    "be"    "real"  "guys"  "i"     "don"   "t"     "know"  "what"  "yolo"  "means"
> make.ngrams(twttxt, 3)
[1] "i m going"       "m going to"      "going to be"     "to be real"      "be real guys"    "real guys i"    
[7] "guys i don"      "i don t"         "don t know"      "t know what"     "know what yolo"  "what yolo means"

set.seed(3535)
presplitter <- function(data, number) {
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
    word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    word
}
pretwitsplit <- presplitter(twitter, 500)

set.seed(3535)
splitter <- function(data, number) {
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
    word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    word <- txt.to.words(word, splitting.rule = "[ ~]")
    word
}
twitsplit <- splitter(twitter, 500)

word <- txt.to.words(word, splitting.rule = "[ ~]")


for(i in 1:length(dim(word)[1])){
    word[i] <- txt.to.words(word[i], splitting.rule = "[ ~]") 
}

r <- as.integer(1:length(dim(word)[1]))
ngrams <- as.data.frame(r)
ngrams$raw[i]

for(i in 1:length(word)){
    r <- as.integer(1:length(word))
    ngrams <- as.data.frame(r)
    ngrams$raw[i] <- txt.to.words(word[i], splitting.rule = "[ ~]") 
}

for(i in 1:length(twit10)){
    r <- as.integer(1:length(twit10))
    ngrams <- as.data.frame(r)
    ngrams$r[i] <- txt.to.words(twit10[i], splitting.rule = "[ ~]") 
}

splitter <- function(data, number) {
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
    word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    allone <- paste(pretwitsplit, collapse = '')
    allw <- txt.to.words(allone, splitting.rule = "[ ~]")
    ngrams <- make.ngrams(allw, 3)
    ngrams
}
twitsplit <- splitter(twitter, 500)

#FUCKIN YATZEE
allone <- paste(pretwitsplit, collapse = '')
allw <- txt.to.words(allone, splitting.rule = "[ ~]")
alln <- make.ngrams(allw, 3)

## NGRAMMIN
ngrammer <- function(data, number) {
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
    word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    allone <- paste(pretwitsplit, collapse = '')
    allw <- txt.to.words(allone, splitting.rule = "[ ~]")
    ngrams <- make.ngrams(allw, 3)
    ngrams
}

##NGRAM AND COUNT -first try, kind of works
ngrammer <- function(data, number) {
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
    word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    allone <- paste(pretwitsplit, collapse = '')
    allw <- txt.to.words(allone, splitting.rule = "[ ~]")
    ngrams <- make.ngrams(allw, 3)
    ngrams
    dcount <- data.frame(table(ngrams))
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount
}

dcount <- dcount[grep(".+", dcount$word), ] #remove blank 
#should we remove periods? dcount <- dcount[-grep("\\.", dcount$word), ]
dashi <- grep("-", dcount$word) #all dashes
gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$word) #good dashes
dmatch <- match(gdashi, dashi) #indices of dashi that are matched by gdashi
bdashi <- dashi[-dmatch] #bad dashes
dcount <- dcount[-bdashi, ] #removes bad dashes
dcount <- dcount[-grep("fuck|shit|damn|crap|bitch", dcount$word, ignore.case=T), ]
dcount <- dcount[order(dcount$Freq, decreasing=T),]

ngrammer <- function(data, number) {
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
    word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    allone <- paste(pretwitsplit, collapse = '')
    allw <- txt.to.words(allone, splitting.rule = "[ ~]")
    ngrams <- make.ngrams(allw, 3)
    ngrams
    dcount <- data.frame(table(ngrams))
    dcount <- dcount[grep(".+", dcount$word), ] #remove blank 
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount
}

### BETTER
ngrammer <- function(data, number) {
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
    word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    allone <- paste(pretwitsplit, collapse = '')
    allw <- txt.to.words(allone, splitting.rule = "[ ~]")
    ngrams <- make.ngrams(allw, 3)
    ngrams
    dcount <- data.frame(table(ngrams))
        dashi <- grep("-", dcount$ngrams)
        gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$ngrams)
        dmatch <- match(gdashi, dashi) #indices of dashi that are matched by gdashi
        bdashi <- dashi[-dmatch]
    dcount <- dcount[-bdashi, ]
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount
}


dashi <- grep("-", dcount$ngrams)
gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$ngrams)
dmatch <- match(gdashi, dashi) #indices of dashi that are matched by gdashi
bdashi <- dashi[-dmatch]
dcount <- dcount[-bdashi, ]

### ALMOST DONE, got rid of bad dashes and multiple puncuations
ngrammer <- function(data, number) {
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
    word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    allone <- paste(word, collapse = '')
    allw <- txt.to.words(allone, splitting.rule = "[ ~]")
    ngrams <- make.ngrams(allw, 3)
    ngrams
    dcount <- data.frame(table(ngrams))
        dashi <- grep("-", dcount$ngrams)
        gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$ngrams)
        dmatch <- match(gdashi, dashi) #indices of dashi that are matched by gdashi
        bdashi <- dashi[-dmatch]
    dcount <- dcount[-bdashi, ]
    dcount <- dcount[-grep("[!\\?#\\.] ?[!\\?#\\.]+", dcount$ngrams), ]
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount
}

dcount <- dcount[grep("[!\\?#\\.] ?[!\\?#\\.]+", dcount$ngrams), ]

## what if I strip all puncuation?  maybe too brutal, but it might improve things
ngrammerNP <- function(data, number) {
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9' -]", "", word) #what we're keeping
    word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    allone <- paste(word, collapse = '')
    allw <- txt.to.words(allone, splitting.rule = "[ ~]")
    ngrams <- make.ngrams(allw, 3)
    ngrams
    dcount <- data.frame(table(ngrams))
    dashi <- grep("-", dcount$ngrams)
    gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$ngrams)
    dmatch <- match(gdashi, dashi) #indices of dashi that are matched by gdashi
    bdashi <- dashi[-dmatch]
    dcount <- dcount[-bdashi, ]
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount
}
twitsmallNP <- ngrammerNP(twitter, 3000)

# also might want to reconsider my apostrophe policy
# maybe just keep contractions as one word

> library(RWeka)
Error : .onLoad failed in loadNamespace() for 'rJava', details:
    call: fun(libname, pkgname)
error: JAVA_HOME cannot be determined from the Registry
In addition: Warning message:
    package 'RWeka' was built under R version 3.1.2 
Error: package or namespace load failed for 'RWeka'

## THE BIG NGRAMMER
ngcount <- function(data, number) {
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
    word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    allone <- paste(word, collapse = '')
    allw <- txt.to.words(allone, splitting.rule = "[ ~]")
    ngrams <- make.ngrams(allw, 3)
    ngrams
    dcount <- data.frame(table(ngrams))
    dashi <- grep("-", dcount$ngrams)
    gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$ngrams)
    dmatch <- match(gdashi, dashi) #indices of dashi that are matched by gdashi
    bdashi <- dashi[-dmatch]
    dcount <- dcount[-bdashi, ]
    dcount <- dcount[-grep("[!\\?#\\.] ?[!\\?#\\.]+", dcount$ngrams), ]
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount$ngrams <- as.character(dcount$ngrams)
    sn <- strsplit(dcount$ngrams, " ")
    #this part takes awhile, even with a twitsmall
    for(i in 1:dim(dcount)[1]){dcount$w1[i] <- sn[[i]][1]}
    for(i in 1:dim(dcount)[1]){dcount$w2[i] <- sn[[i]][2]}
    for(i in 1:dim(dcount)[1]){dcount$w3[i] <- sn[[i]][3]}
    dcount
}


## THE BIG NGRAMMER
ngcount <- function(data, number) {
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
    word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    allone <- paste(word, collapse = '')
    allw <- txt.to.words(allone, splitting.rule = "[ ~]")
    ngrams <- make.ngrams(allw, 3)
    ngrams
    dcount <- data.frame(table(ngrams))
        dashi <- grep("-", dcount$ngrams)
        gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$ngrams)
        dmatch <- match(gdashi, dashi) #indices of dashi that are matched by gdashi
        bdashi <- dashi[-dmatch]
    dcount <- dcount[-bdashi, ]
    dcount <- dcount[-grep("[!\\?#\\.] ?[!\\?#\\.]+", dcount$ngrams), ]
    dcount <- dcount[-grep("#[A-Za-z0-9]+", dcount$ngrams), ]
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount$ngrams <- as.character(dcount$ngrams)
    sn <- strsplit(dcount$ngrams, " ")
    #this part takes awhile, even with a twitsmall
    for(i in 1:dim(dcount)[1]){dcount$w1[i] <- sn[[i]][1]}
    for(i in 1:dim(dcount)[1]){dcount$w2[i] <- sn[[i]][2]}
    for(i in 1:dim(dcount)[1]){dcount$w3[i] <- sn[[i]][3]}
    dcount
}

# WINNER
ngcount <- function(data, number) {
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
    word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    allone <- paste(word, collapse = '')
    allw <- txt.to.words(allone, splitting.rule = "[ ~]", preserve.case = T)
    allw <- allw[-grep("fuck|shit|damn|crap|bitch", allw, ignore.case=T)]
    ngrams <- make.ngrams(allw, 3)
    ngrams
    dcount <- data.frame(table(ngrams))
        dashi <- grep("-", dcount$ngrams)
        gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$ngrams)
        dmatch <- match(gdashi, dashi) #indices of dashi that are matched by gdashi
        bdashi <- dashi[-dmatch]
    dcount <- dcount[-bdashi, ]
    dcount <- dcount[-grep("[!\\?#\\.] ?[!\\?#\\.]+", dcount$ngrams), ]
    dcount <- dcount[-grep("#[A-Za-z0-9]+", dcount$ngrams), ]
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount$ngrams <- as.character(dcount$ngrams)
    sn <- strsplit(dcount$ngrams, " ")
    dcount$w1 <- sapply(sn, function(sn){sn[1]})
    dcount$w2 <- sapply(sn, function(sn){sn[2]})
    dcount$w3 <- sapply(sn, function(sn){sn[3]})
    dcount
}

#this part takes awhile, even with a twitsmall
for(i in 1:dim(dcount)[1]){dcount$w1[i] <- sn[[i]][1]}
for(i in 1:dim(dcount)[1]){dcount$w2[i] <- sn[[i]][2]}
for(i in 1:dim(dcount)[1]){dcount$w3[i] <- sn[[i]][3]}

#this part takes awhile, even with a twitsmall
for(i in 1:dim(dcount)[1]){dcount$w1[i] <- sn[[i]][1]}
for(i in 1:dim(dcount)[1]){dcount$w2[i] <- sn[[i]][2]}
for(i in 1:dim(dcount)[1]){dcount$w3[i] <- sn[[i]][3]}
View(sn)

sapply(sn10, function(sn){
    dcount$w1 <- sn[1]
})

#YES
dcount10$w1 <- sapply(sn, function(sn){sn[1]})

# here we go
dcount$w1 <- sapply(sn, function(sn){sn[1]})
dcount$w2 <- sapply(sn, function(sn){sn[2]})
dcount$w3 <- sapply(sn, function(sn){sn[3]})

ngrammer <- function(data, number) {
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
    word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    allone <- paste(word, collapse = '')
    allw <- txt.to.words(allone, splitting.rule = "[ ~]")
    ngrams <- make.ngrams(allw, 3)
    ngrams
    dcount <- data.frame(table(ngrams))
    dashi <- grep("-", dcount$ngrams)
    gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$ngrams)
    dmatch <- match(gdashi, dashi) #indices of dashi that are matched by gdashi
    bdashi <- dashi[-dmatch]
    dcount <- dcount[-bdashi, ]
    dcount <- dcount[-grep("[!\\?#\\.] ?[!\\?#\\.]+", dcount$ngrams), ]
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount
}


ngrammer <- function(data, number) {
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
    word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    allone <- paste(word, collapse = '')
    allw <- txt.to.words(allone, splitting.rule = "[ ~]")
    ngrams <- make.ngrams(allw, 3)
    ngrams
    dcount <- data.frame(table(ngrams))
    dashi <- grep("-", dcount$ngrams)
    gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$ngrams)
    dmatch <- match(gdashi, dashi) #indices of dashi that are matched by gdashi
    bdashi <- dashi[-dmatch]
    dcount <- dcount[-bdashi, ]
    dcount <- dcount[-grep("[!\\?#\\.] ?[!\\?#\\.]+", dcount$ngrams), ]
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount
}

head(grep("[A-Z]", word))
###breaking it out
data <- twitter
number <- 5000
word <- data[round(runif(number, 0, length(data)+1), 0)]
word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
word <- gsub("(')", "~\\1", word) #split before apostrophe
word <- gsub(".$", "\\1~", word) #split before apostrophe
word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
allone <- paste(word, collapse = '')
####txt.to.words was the culprit!  NEED to do preserve.case=T
allw <- txt.to.words(allone, splitting.rule = "[ ~]", preserve.case=T)
allw <- allw[-grep("fuck|shit|damn|crap|bitch", allw, ignore.case=T)]
ngrams <- make.ngrams(allw, 3)
ngrams
dcount <- data.frame(table(ngrams))
dashi <- grep("-", dcount$ngrams)
gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$ngrams)
dmatch <- match(gdashi, dashi) #indices of dashi that are matched by gdashi
bdashi <- dashi[-dmatch]
dcount <- dcount[-bdashi, ]
dcount <- dcount[-grep("[!\\?#\\.] ?[!\\?#\\.]+", dcount$ngrams), ]
dcount <- dcount[order(dcount$Freq, decreasing=T),]
dcount

allw <- allw[-grep("fuck|shit|damn|crap|bitch", allw, ignore.case=T)]

## USE TWITTER VS. NEWS/BLOGS SKETCH
if (length(grep(" i ", data))*2 > length(grep(" I ", data))) {
    twitcount(data)
} else {
    ngcount(data)
}

### probably want to do a real analysis on the proportion of i to I
icount <- data.frame()


# how many unique words do you need to cover %50 of the word instances in the language? (do they mean the corpus?)
total <- sum(gcount$Freq) #total in corpus

sum(gcount$Freq[1:500])

>sum(gcount$Freq[1:157])/total
[1] 0.0215412

> sum(gcount$Freq[1:8800])/total
[1] 0.1254285

> sum(gcount$Freq==1)/total
[1] 0.4950357


#MAKING PUNCTUATION MORE EFFICIENT
word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !

word <- gsub("([\\.\\?!])", "<>", word) #replace . ? ! with <>
word <- gsub("<>", "~\\1~", word) #split at <>


ngcountNP <- function(data, number) {
    #subsetting the data we want
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    #cleaning puncuation and capitalization
    word <- gsub("([\\.\\?!])", "<>", word) #replace . ? ! with <>
    word <- gsub("<>", "~<>~", word) #split at <>
    word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    # collapsing and ngramming
    allone <- paste(word, collapse = '')
    allw <- txt.to.words(allone, splitting.rule = "[ ~]", preserve.case = T)
    ##filter out profanity
    allw <- allw[-grep("fuck|shit|damn|crap|bitch", allw, ignore.case=T)]
    ngrams <- make.ngrams(allw, 3)
    ngrams
    #compiling counts of ngrams
    dcount <- data.frame(table(ngrams))
    #getting rid of stupid ngrams
    ##dealing with dashes
    dashi <- grep("-", dcount$ngrams)
    gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$ngrams)
    dmatch <- match(gdashi, dashi)
    bdashi <- dashi[-dmatch]
    dcount <- dcount[-bdashi, ]
    ##getting rid of hashtags
    dcount <- dcount[-grep("#[A-Za-z0-9]+", dcount$ngrams), ]
    #ranking ngrams by Freq
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    #splitting them for prediction
    dcount$ngrams <- as.character(dcount$ngrams)
    sn <- strsplit(dcount$ngrams, " ")
    dcount$w1 <- sapply(sn, function(sn){sn[1]})
    dcount$w2 <- sapply(sn, function(sn){sn[2]})
    dcount$w3 <- sapply(sn, function(sn){sn[3]})
    ##getting rid of random unpredictable puncuation
    dcount <- dcount[-grep("<>", dcount$w2),]
    dcount <- dcount[-grep("<>", dcount$w3),]
    dcount
}


doit <- doit[grep("\\.", doit$w2),]

length(grep("\\.", doit$w2))

dcountw2 <- dcount[grep("<>", dcount$w2),]


## RECOMBINING COMPOUND WORDS
ngcountNAp <- function(data, number) {
    #subsetting the data we want
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    #cleaning puncuation and capitalization
    word <- gsub("([\\.\\?!])", "<>", word) #replace . ? ! with <>
    word <- gsub("<>", "~<>~", word) #split at <>
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    # collapsing and ngramming
    allone <- paste(word, collapse = '')
    allw <- txt.to.words(allone, splitting.rule = "[ ~]", preserve.case = T)
    ##filter out profanity
    allw <- allw[-grep("fuck|shit|damn|crap|bitch", allw, ignore.case=T)]
    ngrams <- make.ngrams(allw, 3)
    ngrams
    #compiling counts of ngrams
    dcount <- data.frame(table(ngrams))
    #getting rid of stupid ngrams
    ##dealing with dashes
    dashi <- grep("-", dcount$ngrams)
    gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$ngrams)
    dmatch <- match(gdashi, dashi)
    bdashi <- dashi[-dmatch]
    dcount <- dcount[-bdashi, ]
    ##getting rid of hashtags
    dcount <- dcount[-grep("#[A-Za-z0-9]+", dcount$ngrams), ]
    #ranking ngrams by Freq
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    #splitting them for prediction
    dcount$ngrams <- as.character(dcount$ngrams)
    sn <- strsplit(dcount$ngrams, " ")
    dcount$w1 <- sapply(sn, function(sn){sn[1]})
    dcount$w2 <- sapply(sn, function(sn){sn[2]})
    dcount$w3 <- sapply(sn, function(sn){sn[3]})
    ##getting rid of random unpredictable puncuation
    dcount <- dcount[-grep("<>", dcount$w2),]
    dcount <- dcount[-grep("<>", dcount$w3),]
    dcount
}

sum(doitNAp$Freq==1)/sum(doitNAp$Freq)
[1] 0.7538611

sum(doitNAp$Freq==1)/sum(doitNAp$Freq)

doitNAp2 <- doitNAp[doitNAp$Freq>1,]

## THE CONSTRUCTOR
### works, but will build2 run with 2.5m rows?  I guess we won't know till we try it.
build1 <- function(data, number) {
    #subsetting the data we want
    word <- data[round(runif(number, 0, length(data)+1), 0)]
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    #cleaning puncuation and capitalization
    word <- gsub("([\\.\\?!])", "<>", word) #replace . ? ! with <>
    word <- gsub("<>", "~<>~", word) #split at <>
    word <- gsub("([A-Z][A-Z][A-Z][A-Z]+)", "\\L\\1", word, perl=TRUE) #lower 4 running caps
    # collapsing and ngramming
    allone <- paste(word, collapse = '')
    allw <- txt.to.words(allone, splitting.rule = "[ ~]", preserve.case = T)
    ##filter out profanity
    allw <- allw[-grep("fuck|shit|damn|crap|bitch", allw, ignore.case=T)]
    ngrams <- make.ngrams(allw, 3)
    #compiling counts of ngrams
    dcount <- data.frame(table(ngrams))
    #getting rid of stupid ngrams
    ##dealing with dashes
    dashi <- grep("-", dcount$ngrams)
    gdashi <- grep("[A-Za-z0-9]+-[A-Za-z0-9]+", dcount$ngrams)
    dmatch <- match(gdashi, dashi)
    bdashi <- dashi[-dmatch]
    dcount <- dcount[-bdashi, ]
    ##getting rid of hashtags
    dcount <- dcount[-grep("#[A-Za-z0-9]+", dcount$ngrams), ]
    dcount
}
data1 <- build1(twitter, 10000)
data2 <- build1(twitter, 10000)
#...
rawdata <- rbind(data1, data2)

build2 <- function(data){
    #aggregate Freq sums
    dcount <- aggregate(data$Freq, list(data$ngrams), FUN="sum")
    names(dcount) <- c("ngrams", "Freq")
    #ranking ngrams by Freq
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    #splitting them for prediction
    dcount$ngrams <- as.character(dcount$ngrams)
    sn <- strsplit(dcount$ngrams, " ")
    dcount$w1 <- sapply(sn, function(sn){sn[1]})
    dcount$w2 <- sapply(sn, function(sn){sn[2]})
    dcount$w3 <- sapply(sn, function(sn){sn[3]})
    ##getting rid of random unpredictable puncuation
    dcount <- dcount[-grep("<>", dcount$w2),]
    dcount <- dcount[-grep("<>", dcount$w3),]
    #get rid of singletons
    dcount <- dcount[dcount$Freq>1,]
    dcount
}

fulldata <- build2(rawdata)

## BACK OFF (PSEUDO?)
sentence <- "I could never be loyal to you."

w2 <- grep("[A-Za-z1-9]+$", s1, value=TRUE)



words <- function(word) {
word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
word <- unlist(strsplit(word, "[ ~]")) #actually make the split
word
}

> s1[length(s1)]
[1] "you"
> s1[length(s1)-1]
[1] "to"
> s1[length(s1)-2]
[1] "loyal"


words <- function(word) {
    word <- gsub("[^A-Za-z0-9'# -]", "", word) #what we're keeping
    word <- unlist(strsplit(word, "[ ]")) #actually make the split
    word
}

## FIRST TRY AT FULL THING
pred1 <- function(word) {
    preds <- datafull
    word <- gsub("[^A-Za-z0-9'# -]", "", word) #what we're keeping
    words <- unlist(strsplit(word, "[ ]")) #actually make the split
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



# WORD PICKER
predtable <- function(sentence) {
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
    }else{preds <- preds; word2 <- words[length(words)]}
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
    View(preds)
}


# with fixed=T
predtableF <- function(sentence) {
    preds <- datafull
    sentence <- gsub("[^A-Za-z0-9'# -]", "", sentence) #what we're keeping
    words <- unlist(strsplit(sentence, "[ ]")) #actually make the split
    word2 <- words[length(words)]
    if(length(grep(word2, preds$w2, fixed=T))>0){
        preds <- preds[grep(word2, preds$w2, fixed=T), ]
    }else{word2 <- words[length(words)-1]}
    if(length(grep(word2, preds$w2, fixed=T))>0){
        preds <- preds[grep(word2, preds$w2, fixed=T), ]
    }else{word2 <- words[length(words)-2]}
    if(length(grep(word2, preds$w2, fixed=T))>0){
        preds <- preds[grep(word2, preds$w2, fixed=T), ]
    }else{preds <- preds; word2 <- words[length(words)]}
    word1 <- words[max(grep(word2, words))-1]
    if(length(grep(word1, preds$w1, fixed=T))>0){
        preds <- preds[grep(word1, preds$w1, fixed=T), ]
    }else{word1 <- words[max(grep(word2, words))-2]}
    if(length(grep(word1, preds$w1, fixed=T))>0){
        preds <- preds[grep(word1, preds$w1, fixed=T), ]
    }else{word1 <- words[max(grep(word2, words))-3]}
    if(length(grep(word1, preds$w1, fixed=T))>0){
        preds <- preds[grep(word1, preds$w1, fixed=T), ]
    }else{preds <- preds}
    preds
}

# with ==
predtableE <- function(sentence) {
    preds <- datafull
    sentence <- gsub("[^A-Za-z0-9'# -]", "", sentence) #what we're keeping
    words <- unlist(strsplit(sentence, "[ ]")) #actually make the split
    word2 <- words[length(words)]
    if(length(grep(word2, preds$w2))>0){
        preds <- preds[preds$w2==word2, ]
    }else{word2 <- words[length(words)-1]}
    if(length(grep(word2, preds$w2))>0){
        preds <- preds[preds$w2==word2, ]
    }else{word2 <- words[length(words)-2]}
    if(length(grep(word2, preds$w2))>0){
        preds <- preds[preds$w2==word2, ]
    }else{preds <- preds; word2 <- words[length(words)]}
    word1 <- words[max(grep(word2, words))-1]
    if(length(grep(word1, preds$w1))>0){
        preds <- preds[preds$w1==word1, ]
    }else{word1 <- words[max(grep(word2, words))-2]}
    if(length(grep(word1, preds$w1))>0){
        preds <- preds[preds$w1==word1, ]
    }else{word1 <- words[max(grep(word2, words))-3]}
    if(length(grep(word1, preds$w1))>0){
        preds <- preds[preds$w1==word1, ]
    }else{preds <- preds}
    View(preds)
}

## more exact but fucked it up
predtableE <- function(sentence) {
    preds <- datafull
    sentence <- gsub("[^A-Za-z0-9'# -]", "", sentence) #what we're keeping
    words <- unlist(strsplit(sentence, "[ ]")) #actually make the split
    word2 <- words[length(words)]
    if(dim(preds[preds$w2==word2, ])[1]>0){
        preds <- preds[preds$w2==word2, ]
    }else{word2 <- words[length(words)-1]}
    if(dim(preds[preds$w2==word2, ])[1]>0){
        preds <- preds[preds$w2==word2, ]
    }else{word2 <- words[length(words)-2]}
    if(dim(preds[preds$w2==word2, ])[1]>0){
        preds <- preds[preds$w2==word2, ]
    }else{preds <- preds; word2 <- words[length(words)]}
    word1 <- words[max(grep(word2, words))-1]
    if(dim(preds[preds$w1==word1, ])[1]>0){
        preds <- preds[preds$w1==word1, ]
    }else{word1 <- words[max(grep(word2, words))-2]}
    if(dim(preds[preds$w1==word1, ])[1]>0){
        preds <- preds[preds$w1==word1, ]
    }else{word1 <- words[max(grep(word2, words))-3]}
    if(dim(preds[preds$w1==word1, ])[1]>0){
        preds <- preds[preds$w1==word1, ]
    }else{preds <- preds}
    View(preds)
}


## problem is here:
> word1
[1] "boss"
> dim(preds[preds$w1==word1, ])[1]
[1] 2
> preds[preds$w1==word1, ]
X ngrams Freq   w1   w2   w3
NA   NA   <NA>   NA <NA> <NA> <NA>
    NA.1 NA   <NA>   NA <NA> <NA> <NA>
    > 
    
    
    # I don't get it
    
#fixing it with na.omit
predtableE <- function(sentence) {
    preds <- datafull
    sentence <- gsub("[^A-Za-z0-9'# -]", "", sentence) #what we're keeping
    words <- unlist(strsplit(sentence, "[ ]")) #actually make the split
    word2 <- words[length(words)]
    if(dim(na.omit(preds[preds$w2==word2, ]))[1]>0){
        preds <- preds[preds$w2==word2, ]
    }else{word2 <- words[length(words)-1]}
    if(dim(na.omit(preds[preds$w2==word2, ]))[1]>0){
        preds <- preds[preds$w2==word2, ]
    }else{word2 <- words[length(words)-2]}
    if(dim(na.omit(preds[preds$w2==word2, ]))[1]>0){
        preds <- preds[preds$w2==word2, ]
    }else{preds <- preds; word2 <- words[length(words)]}
    word1 <- words[max(grep(word2, words))-1]
    if(dim(na.omit(preds[preds$w1==word1, ]))[1]>0){
        preds <- preds[preds$w1==word1, ]
    }else{word1 <- words[max(grep(word2, words))-2]}
    if(dim(na.omit(preds[preds$w1==word1, ]))[1]>0){
        preds <- preds[preds$w1==word1, ]
    }else{word1 <- words[max(grep(word2, words))-3]}
    if(dim(na.omit(preds[preds$w1==word1, ]))[1]>0){
        preds <- preds[preds$w1==word1, ]
    }else{preds <- preds}
    View(na.omit(preds))
}

######NON-NGRAM METHODS#######

## TOKENIZING for term-doc matrix
blog30 <- blogs[100:129]
word <- blog30
toker <- function(word){
    word <- gsub("[^A-Za-z0-9!\\?'#\\. -]", "", word) #what we're keeping
    word <- gsub("([\\.\\?!])", "~\\1~", word) #split at . ? !
    #word <- gsub("(')", "~\\1", word) #split before apostrophe
    word <- strsplit(word, "[ ~]") #actually make the split
    word
}

## do the ngrams (2) and tokenizer on the dame data and then
### compare how many times a word is in a pair vs. not in that pair
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

set.seed(3030)
blog1000 <- blogs[round(runif(1000, 0, length(blogs)+1), 0)]
ng <- ng2(blog1000)
tk <- ng2toke(blog1000)

## WINNER
ng$Freq[2] / (tk$Freq[tk$word==ng$w1[2]] + tk$Freq[tk$word==ng$w2[2]])

# for loop works, but a little slow
# 30,000 rows took about 2 or 3 minutes
for(i in 1:dim(ng)[1]) {
    ng$rate[i] <- ng$Freq[i] / (tk$Freq[tk$word==ng$w1[i]] + tk$Freq[tk$word==ng$w2[i]])
}
goodng <- ng[ng$rate>.15, ]

#good, but fails on a non-match
twoGram <- function(ng, tk) {
    for(i in 1:dim(ng)[1]) {
        ng$rate[i] <- ng$Freq[i] / 
            (tk$Freq[tk$word==ng$w1[i]] + tk$Freq[tk$word==ng$w2[i]])
    }
    goodng <- ng[ng$rate>.15, ]
    goodng
}

#fixing non-match with if then...
twoGram <- function(ng, tk) {
    for(i in 1:dim(ng)[1]) {
        dnm <- as.numeric(tk$Freq[tk$word==ng$w1[i]] + tk$Freq[tk$word==ng$w2[i]])
        ifelse(dnm > 0, dnm, 1)
        ng$rate[i] <- as.numeric(ng$Freq[i] / dnm)
    }
    goodng <- ng[ng$rate>.15, ]
    goodng
}
# tried using ng$Freq[i] instead of 1 in the ifelse, but it failed
# should work now, but it takes forever (10 minutes) on 10,000 ng's

#FIXED NON-MATCH with if then and LENGTH...
twoGram <- function(ng, tk) {
    for(i in 1:dim(ng)[1]) {
        tkw1 <- ifelse(length(tk$Freq[tk$word==ng$w1[i]]) > 0, 
                       tk$Freq[tk$word==ng$w1[i]], 0)
        tkw2 <- ifelse(length(tk$Freq[tk$word==ng$w2[i]]) > 0,
                       tk$Freq[tk$word==ng$w2[i]], 0)
        dnm <- tkw1 + tkw2
        dnm <- ifelse(dnm >= 1, dnm, ng$Freq[i])
        ng$rate[i] <- ng$Freq[i] / dnm
    }
    goodng <- ng[ng$rate>.15, ]
    goodng
}

#works!
tkw1 <- function(i)ifelse(length(tk$Freq[tk$word==ng$w1[i]]) > 0, 
               tk$Freq[tk$word==ng$w1[i]], 0)

# can't get apply to work, maybe revisit later
apply (ng, 1, function(tk, ng) {
    ng$rate <- ng$Freq / (tk$Freq[tk$word==ng$w1] + tk$Freq[tk$word==ng$w2])
})

ng$rate <- apply(ng, 1, function(ng,tk) {ng$Freq / (tk$Freq[tk$word==ng$w1] + tk$Freq[tk$word==ng$w2])})

#REVISITING APPLY
TGrate <- function(ng, w1, w2, Freq) {
    dnm <- (tk$Freq[tk$word==w1] + tk$Freq[tk$word==w2])
    ifelse(dnm > 0, dnm, 1)
    rate <- as.numeric(Freq) / as.numeric(dnm)
    rate
}
ng$rate <- apply(ng, 1, TGrate, w1="w1", w2="w2", Freq="Freq")
# ALMOST, but getting this
#Error in `$<-.data.frame`(`*tmp*`, "rate", value = numeric(0)) : 
#    replacement has 0 rows, data has 15492
#In addition: There were 50 or more warnings (use warnings() to see the first 50)
#Error during wrapup: cannot open the connection
goodng <- ng[ng$rate>.15, ]
goodng

# testing new apply
set.seed(3030)
blog500 <- blogs[round(runif(500, 0, length(blogs)+1), 0)]
ng <- ng2(blog500)
tk <- ng2toke(blog500)

##debugging apply
TGrate <- function(ng, w1, w2, Freq) {
    dnm <- (tk$Freq[tk$word==w1] + tk$Freq[tk$word==w2])
    ifelse(dnm > 0, dnm, 1)
    rate <- as.numeric(Freq) / as.numeric(dnm)
    rate
}
ng$rate <- apply(ng, 1, TGrate, w1="w1", w2="w2", Freq="Freq")

#
TGfreq <- function(w1) {tk$Freq[tk$word==w1]}
apply(ng5, 1, TGfreq, w1="w1")

#
bob<- 5
TGbob <- function(Freq) {as.numeric(Freq) + bob}
apply(ng5, 1, TGbob)

#
TGrate <- function(ng) {
    dnm <- (tk$Freq[tk$word==ng[4]] + tk$Freq[tk$word==ng[5]])
    ifelse(dnm > 0, dnm, 1)
    rate <- as.numeric(ng[3]) / as.numeric(dnm)
    rate
}
ng5$rate <- apply(ng5, 1, TGrate)

#
TGrate <- function(ng, w1, w2, Freq){
    tkw1 <- ifelse(length(tk$Freq[tk$word==w1]) > 0, 
               tk$Freq[tk$word==w1], 0)
    tkw2 <- ifelse(length(tk$Freq[tk$word==w2]) > 0,
               tk$Freq[tk$word==w2], 0)
    dnm <- tkw1 + tkw2
    dnm <- ifelse(dnm >= 1, dnm, Freq)
    rate <- as.numeric(Freq) / as.numeric(dnm)
    rate
}
apply(ng5, 1, TGrate, w1="w1", w2="w2", Freq="Freq")

### idea 2.1 from Stack Overflow
names(tk)[2] <- 'tkfreq'
ng <- merge(ng,tk,by.x = 'w2',by.y = 'word',all.x = T)
ng <- merge(ng,tk,by.x = 'w1',by.y = 'word',all.x = T)
ng$tkfreq.x[is.na(ng$tkfreq.x)] <- 0
ng$tkfreq.y[is.na(ng$tkfreq.y)] <- 0
ng$dnm <- ng$tkfreq.x + ng$tkfreq.y
ng$rate <- ng$Freq / ng$dnm
ng[is.infinite(ng$rate),'rate'] <- 1

## WINNER
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

#
ng20 <- merge(ng20,tk20,by.x = 'w1',by.y = 'word',all.x = T)
ng20 <- merge(ng20,tk20,by.x = 'w2',by.y = 'word',all.x = T)



## DELving INTO TM
> library(tm)
Loading required package: NLP
Warning messages:
    1: package 'tm' was built under R version 3.1.2 
2: package 'NLP' was built under R version 3.1.2 
> b1000vc <- VCorpus(VectorSource(blog1000))
> b1000vc
<<VCorpus (documents: 1000, metadata (corpus/indexed): 0/0)>>
    > b1000tdm <- TermDocumentMatrix(b1000vc)

## BUILDING A DICTIONARY
set.seed(3030)
blog10k <- blogs[round(runif(10000, 0, length(blogs)+1), 0)]
b10kToke <- ng2toke(blog10k)
b10kGram <- ng2(blog10k)
b10k2g <- good2g(b10kGram, b10kToke)

ng <- b10kGram
tk <- b10kToke
tg <- b10k2g

tk3 <- tk[tk$Freq>2,]
tg3 <- tg[tg$Freq>2,]
tg3 <- tg3[, c("ngrams", "Freq")]
names(tg3) <- c("word", "Freq")
dict <- rbind(tg3, tk3)
dict <- dict[order(dict$Freq, decreasing=T),]
dict <- dict[-1,]

#### open dict.R and run all the functions, then run this
blogcon <- file("./Coursera-Swiftkey/final/en_US/en_US.blogs.txt", "rb")
blogs <- readLines(blogcon)
close(blogcon)

set.seed(3030)
blog10k <- blogs[round(runif(10000, 0, length(blogs)+1), 0)]
b10kToke <- ng2toke(blog10k)
b10kGram <- ng2(blog10k)
dict <- dict(b10kGram, b10kToke)

# then read the tm doc and go to town
vb10k <- VCorpus(VectorSource(blog10k))
words <- dict$word
dtm10k <- DocumentTermMatrix(vb10k, list(dictionary=words)) #took 2 minutes
inspect(dtm10k[1:100, 1:50]) #hard to see anything


# try with smaller
blog100 <- blogs[round(runif(100, 0, length(blogs)+1), 0)]
vb100 <- VCorpus(VectorSource(blog100))
words <- dict$word
words200 <- words[1:200]
dtm100 <- DocumentTermMatrix(vb100, list(dictionary = words200))
dtm100w <- DocumentTermMatrix(vb100, list(dictionary=words))
View(inspect(dtm100))

## still have to work on if this is scalable
blog100k <- blogs[round(runif(100000, 0, length(blogs)+1), 0)]
vb100k <- VCorpus(VectorSource(blog100k))
words2k <- words[1:2000]
dtm100k <- DocumentTermMatrix(vb100k, list(dictionary=words2k)) # took 5 mins

> findAssocs(dtm100k, "movie", 0.2)
movie
independent  0.29
fat          0.26
movies       0.22
> length(findAssocs(dtm100k, "movie", 0.2))
[1] 3

# consider stemming and then culling anything with length(findAssocs) < 2
