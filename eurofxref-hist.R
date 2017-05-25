dataset <- read.csv("eurofxref-hist.csv",header = TRUE)
dataset <- data.frame(dataset$GBP,dataset$USD,dataset$AUD,dataset$DKK,dataset$HKD)
cl <-dataset
library(corrplot)
corrplot(cor(cl))