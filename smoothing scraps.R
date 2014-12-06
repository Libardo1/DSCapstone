library(dplyr)


predictor("but", "I", "want")
#[1] "but I want to" "to" 

but <- filter(predsfull, predsfull$w1=="but"|predsfull$w2=="but"|predsfull$w3=="but"|predsfull$w4=="but")
#butw1 <- but[but$w1=="but",]
#butw1 <- butw1[complete.cases(butw1), ]
#or this, but it's ugly: butw1 <- but[but$w1=="but",][complete.cases(but[but$w1=="but",]), ]
butw1p <- sum(but$Freq[but$w1=="but"], na.rm=T)
butall <- sum(but$Freq, na.rm=T)
butw1p/butall

I <- filter(predsfull, predsfull$w1=="I"|predsfull$w2=="I"|predsfull$w3=="I"|predsfull$w4=="I")
Ibut <- rbind(I[I$w1=="but" & I$w2=="I", ], 
              I[I$w2=="but" & I$w3=="I", ], 
              I[I$w3=="but" & I$w4=="I", ])
#Ibut <- Ibut[complete.cases(Ibut),]
Ibutp <- sum(Ibut$Freq, na.rm=T)
Iall <- sum(I$Freq, na.rm=T)
Ibutp/Iall

want <- filter(predsfull, predsfull$w1=="want"|predsfull$w2=="want"|predsfull$w3=="want"|predsfull$w4=="want")
wantI <- rbind(I[I$w1=="I" & I$w2=="want", ], 
              I[I$w2=="I" & I$w3=="want", ], 
              I[I$w3=="I" & I$w4=="want", ])
#Ibut <- Ibut[complete.cases(Ibut),]
wantIp <- sum(wantI$Freq, na.rm=T)
wantall <- sum(want$Freq, na.rm=T)
wantIp/wantall


