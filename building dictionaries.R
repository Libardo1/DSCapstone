#load dictionaries
blogTrdict <- read.csv("blogTrdict.csv")
twitTrdict <- read.csv("twitTrdict.csv")
newsTrdict <- read.csv("newsTrdict.csv")
nbTrdict <- read.csv("nbTrdict.csv")
fullTrdict <- read.csv("fullTrdict.csv")
fullDict100 <- read.csv("fullDict100.csv")
#load data
blogTrcon <- file("blogTr.txt", "rb")
blogTr <- readLines(blogTrcon)
close(blogTrcon)
newsTrcon <- file("newsTr.txt", "rb")
newsTr <- readLines(newsTrcon)
close(newsTrcon)
twitTrcon <- file("twitTr.txt", "rb")
twitTr <- readLines(twitTrcon)
close(twitTrcon)
#build dtm
set.seed(3030)
blog33 <- blogTr[round(runif(3300, 1, length(blogTr)), 0)]
twit33 <- twitTr[round(runif(3300, 1, length(twitTr)), 0)]
news33 <- newsTr[round(runif(3300, 1, length(newsTr)), 0)]
full33 <- c(blog33, twit33, news33)
corp33 <- VCorpus(VectorSource(full33))
words100 <- fullDict100$word
dtm33 <- DocumentTermMatrix(corp33, list(dictionary = words100)) # took 3 mins, 3.4 Mb



###BUILDING
## BLOGS
blogcon <- file("./Coursera-Swiftkey/final/en_US/en_US.blogs.txt", "rb")
blogs <- readLines(blogcon)
close(blogcon)

set.seed(3030)
blogTrain <- round(runif(round(length(blogs)*0.2, 0), 1, length(blogs)), 0)
blogTr <- blogs[blogTrain]
blogTe <- blogs[-blogTrain]

blogTrng <- ng2(blogTr) #about 3 minutes
blogTrtk <- ng2toke(blogTr) #about 45 seconds
blogTrdict <- dict(blogTrng, blogTrtk) #about 1 minute

## TWITTER
twitcon <- file("./Coursera-Swiftkey/final/en_US/en_US.twitter.txt", "rb")
twitter <- readLines(twitcon)
close(twitcon)

set.seed(3030)
twitTrain <- round(runif(round(length(twitter)*0.2, 0), 1, length(twitter)), 0)
twitTr <- twitter[twitTrain]
twitTe <- twitter[-twitTrain]

twitTrng <- ng2(twitTr) #about 3 minutes
twitTrtk <- ng2toke(twitTr) #about 30 seconds
twitTrdict <- dict(twitTrng, twitTrtk) #about 1 minute

## NEWS
newscon <- file("./Coursera-Swiftkey/final/en_US/en_US.news.txt", "rb")
news <- readLines(newscon)
close(newscon)

set.seed(3030)
newsTrain <- round(runif(round(length(news)*0.2, 0), 1, length(news)), 0)
newsTr <- news[newsTrain]
newsTe <- news[-newsTrain]

newsTrng <- ng2(newsTr) #about 3 minutes
newsTrtk <- ng2toke(newsTr) #about 30 seconds
newsTrdict <- dict(newsTrng, newsTrtk) #about 1 minute

## COMBINING
nbTrdict <- rbind(blogTrdict, newsTrdict)
fullTrdict <- rbind(blogTrdict, twitTrdict, newsTrdict)

compile <- function(dcount){
    #aggregate Freq sums
    dcount <- aggregate(dcount$Freq, list(dcount$word), FUN="sum")
    names(dcount) <- c("word", "Freq")
    #ranking ngrams by Freq
    dcount <- dcount[order(dcount$Freq, decreasing=T),]
    dcount$word <- as.character(dcount$word)
    dcount
}

nbTrdict <- compile(nbTrdict)
fullTrdict <- compile(fullTrdict)

write.csv(blogTrdict, "blogTrdict.csv")
write.csv(twitTrdict, "twitTrdict.csv")
write.csv(newsTrdict, "newsTrdict.csv")
write.csv(nbTrdict, "nbTrdict.csv")
write.csv(fullTrdict, "fullTrdict.csv")


#> dim(fullTrdict[fullTrdict$Freq>5,])
#[1] 70309     2
#> dim(fullTrdict[fullTrdict$Freq>10,])
#[1] 50138     2
#> dim(fullTrdict[fullTrdict$Freq>20,])
#[1] 34474     2
#> dim(fullTrdict[fullTrdict$Freq>50,])
#[1] 19944     2
fullDict50 <- fullTrdict[fullTrdict$Freq>50,]
#> dim(fullTrdict[fullTrdict$Freq>100,])
#[1] 12660     2
fullDict100 <- fullTrdict[fullTrdict$Freq>100,]
fullDict100$word <- as.character(fullDict100$word)
words100 <- fullDict100$word

## NOW BUILD SOME CORPI
fullTr <- c(blogTr, twitTr, newsTr)
fullCorpus <- VCorpus(VectorSource(fullTr)) # took almost 7 minutes, prob too big

# 9900 docs, 12000 words
set.seed(3030)
blog33 <- blogTr[round(runif(3300, 1, length(blogTr)), 0)]
twit33 <- twitTr[round(runif(3300, 1, length(twitTr)), 0)]
news33 <- newsTr[round(runif(3300, 1, length(newsTr)), 0)]
full33 <- c(blog33, twit33, news33)
corp33 <- VCorpus(VectorSource(full33))
dtm33 <- DocumentTermMatrix(corp33, list(dictionary = words100)) # took 3 mins, 3.4 Mb

# 30000 docs, 6600 words
fullDict250 <- fullTrdict[fullTrdict$Freq>250,]
words250 <- fullDict250$word
set.seed(3030)
blog30 <- blogTr[round(runif(10000, 1, length(blogTr)), 0)]
twit30 <- twitTr[round(runif(10000, 1, length(twitTr)), 0)]
news30 <- newsTr[round(runif(10000, 1, length(newsTr)), 0)]
full30 <- c(blog30, twit30, news30)
corp30 <- VCorpus(VectorSource(full30))
dtm30 <- DocumentTermMatrix(corp30, list(dictionary = words250)) # took 5 mins, 8.1 Mb
#tried 30k by 6600
#tried 15k by 6600
#tried 4.5k by 12000
# none as good as the original: 9900 by 12000

#trying to save it for later use
dtm33DF <- as.data.frame(inspect(dtm33), stringsAsFactors = FALSE)
write.table(dtm33DF)
# fail

Assoc <- function(data) {
    point <- numeric(0)
    for(i in 1:length(data$word)) {
        point[i] <- length(findAssocs(dtm33, data$word[i], 0.2))
    }
    point
}
# also, too slow.  Took 90 seconds to run 20 words

length(findAssocs(dtm33, fullDict100$word[1], 0.2))

list100 <- findAssocs(dtm33, fullDict100$word[1:100], 0.2) #realllly slow! 5 mins or so just to run 100
point2 <- as.numeric(sapply(list100, length))

## STOPWORDS
corp33sw <- tm_map(corp33, removeWords, stopwords("english"))
dtm33sw <- DocumentTermMatrix(corp33sw, list(dictionary = words100)) # took 3 mins, 3.4 Mb
#useless


