install.packages("tm")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("hash")

library(tm)
library(ggplot2)

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
## find terms with a frequency of 5 or more
ACQtdm2 <- TermDocumentMatrix(ACQstop, control = list(wordLengths = c(1, Inf)))
ACQtdm2
freq.terms <- findFreqTerms(ACQtdm2, lowfreq = 5)
freq.terms
## find words associated with "states"
findAssocs(ACQtdm2, "states", 0.25)
## term frequency
term.freq <- rowSums(as.matrix(ACQtdm2))
term.freq <- subset(term.freq, term.freq >= 5)
df <- data.frame(term = names(term.freq), freq = term.freq)
term.freq
df


###########################################
## Q(b) use inspect
## 15 largest document
## 50:1068, 47:3013, 44:1022, 42:1607, 36:1043, 34:1465, 29:3109, 25:3516, 22:1873, 20:1009, 19:2457
## 18:871, 7:3635, 4:2308, 1:1287
inspect(acq)

############################################
## Q(c) 
## dendrogram
tdm2 <- removeSparseTerms(ACQtdm2, sparse = 0.50)
tdm2
dd <- dist(scale(tdm2), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc)
## WordCloud
m1 <- as.matrix(tdm2)
word.freq <- sort(rowSums(m1), decreasing = T)
word.freq
library(wordcloud)
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, random.order = F, colors = pal)

######################################################
install.packages("textreuse")
install.packages("wordnet")
install.packages("zipfR")
install.packages("tidyverse")
install.packages("tokenizers")
## see the content of document
as.character(acq[[7]])
## Q(d)
library(textreuse)
library(tidyverse)
library(tokenizers)
## get one of 15 largest documents
docI <- acq[[7]]
charDoc <- as.character(docI)
# print longest word in file
max_word_len = 0
max_word = ""
for (word in tokenize_words(charDoc)){
  print(word)
  print(nchar(word))
  print(max_word_len)
  if(nchar(word) > max_word_len){
    max_word = word
    max_word_len = nchar(word)
  }
}
print(max_word)
# print the longest sentence in every file
max_sentence_len = 0
max_sentence = ""
for (line in tokenize_sentences(charDoc)){
  count = 0
  for (words in tokenize_words(line)){
    count=count+1
  }
  print(line)
  if(count > max_len){
    max_sentence = line
    max_sentence_len = count
  }
}
print(max_sentence)

#######################################################
## Q(e)
## draw a table show the length of longest sentence
length_array <- c(15, 16)    ## change length here
length_data <- data.frame(len = length_array[1:2])
mytable <- cbind(sites = c("file 1", "file 2"), length_data[1:2,])   ## change file name here
rownames(mytable) <- c("No1", "No2")

#############################################################
## Q(f)
## remove punctuation
fileNoPun <- tm_map(acq, content_transformer(removeNumPunct))
DocINoPun <- fileNoPun[[7]]
tokenize_sentences(as.character(DocINoPun))


#############################################################
## Q(g)
## print part of speech of every word
library(wordnet)
docI <- acq[[7]]
charDoc <- as.character(docI)
sentences <- tokenize_sentences(charDoc)
sentences_words <- sapply(sentences, tokenize_words)
sentences


##############################################################
## Q(h)
## print word frequency
library(zipfR)
testFre <- termFreq(acq[[7]])
rt_pos = as.data.frame(testFre)
Vm = rt_pos[1]
testFre

