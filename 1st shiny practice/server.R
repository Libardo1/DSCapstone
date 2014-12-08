library(stylo)

#predsfull <- read.csv("./pdata/predsfull.csv", stringsAsFactors=F)

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

#spinsFans <- function(spins) spins^1.2
#likesFans <- function(likes) likes/3


shinyServer(
    function(input, output) {
        pick <- reactive({bigPicker(input$words)})
        #sp <- reactive({spinsFans(input$spins)})
        #li <- reactive({likesFans(input$likes)})
        output$pick <- renderText({
            input$goButton
            isolate(pick())})
        #output$spinspt <- renderPrint({round(sp(), 2)})
        #output$likespt <- renderPrint({round(li(), 2)})
        
    }
)

