install.packages("tm")

library(tm)

## corpus of 50 documents
DocData<-data("acq")

########################################
## Q(a) try function in lecture
inspect(acq)
## the length of specific document
test11<-acq[[1]]
test11
## sparsity/ Max length term
ACQdtm<-DocumentTermMatrix(acq)
ACQdtm
## inspect term
inspect(ACQdtm[1:15, 1:6])
## frequency of term
test1tf <- termFreq(test11)
test1tf
## convert to a dataFrame
test1df <- as.data.frame(test1tf)
test1df
## Convert the corpus to lower case
ACQlow<- tm_map(acq, content_transformer(tolower))
ACQlow
## remove anything other than English letters or spaces
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
ACQcl <- tm_map(ACQlow, content_transformer(removeNumPunct))
## remove stop words from the corpus
myStopword <- c(stopwords('english'))
ACQstop <- tm_map(ACQcl, removeWords, myStopword)
inspect(ACQstop[1:2])


###########################################

writeCorpus(DocData, path = "")

setwd("Documents/GitHub/Text-Analytivs")
SAT<-VCorpus(DirSource(".", ignore.case = TRUE, mode="text"))
inspect(SAT)