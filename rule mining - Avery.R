library(tidyverse)
library(arules)

groceries <-read.transactions("groceries.txt", format = "basket", sep = ",")

groc = apriori(groceries, 
                     parameter=list(support=.005, confidence=.1, maxlen=5))

inspect(groc)
