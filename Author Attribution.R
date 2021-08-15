## The tm library and related plugins comprise R's most popular text-mining stack.
## See http://cran.r-project.org/web/packages/tm/vignettes/tm.pdf

rm(list=ls())
library(NLP)
library(tm) 
library(tidyverse)
library(slam)
library(proxy)
library(caret)


## tm has many "reader" functions.  Each one has
## arguments elem, language, id
## (see ?readPlain, ?readPDF, ?readXML, etc)
## This wraps another function around readPlain to read
## plain text documents in English.

# I've stored this function as a Github "gist" at:
# https://gist.github.com/jgscott/28d9d1287a0c3c1477e2113f6758d5ff

readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }


### TRAIN DATA #####
author_dirs_train = Sys.glob('data/ReutersC50/C50train/*')

file_list_train = NULL
labels_train = NULL
for(author_train in author_dirs_train) {
  author_name_train = tail(strsplit(author_train,split="/")[[1]],1)
  files_to_add_train = Sys.glob(paste0(author_train, '/*.txt'))
  file_list_train = append(file_list_train, files_to_add_train)
  labels_train = append(labels_train, rep(author_name_train, length(files_to_add_train)))
}


# Need a more clever regex to get better names here
train_docs = lapply(file_list_train, readerPlain) 
names(train_docs) = file_list_train
names(train_docs) = sub('.txt', '', names(train_docs))
my_corpus_train = Corpus(VectorSource(train_docs))

my_corpus_train = tm_map(my_corpus_train, content_transformer(tolower)) # make everything lowercase
my_corpus_train = tm_map(my_corpus_train, content_transformer(removeNumbers)) # remove numbers
my_corpus_train = tm_map(my_corpus_train, content_transformer(removePunctuation)) # remove punctuation
my_corpus_train = tm_map(my_corpus_train, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus_train = tm_map(my_corpus_train, content_transformer(removeWords), stopwords("SMART"))

DTM_train = DocumentTermMatrix(my_corpus_train)
DTM_train = removeSparseTerms(DTM_train, 0.975)

train_data = as.data.frame(as.matrix(DTM_train))


### TEST DATA #####
author_dirs_test = Sys.glob('data/ReutersC50/C50test/*')

file_list_test = NULL
labels_test = NULL
for(author_test in author_dirs_test) {
  author_name_test = tail(strsplit(author_test,split="/")[[1]],1)
  files_to_add_test = Sys.glob(paste0(author_test, '/*.txt'))
  file_list_test = append(file_list_test, files_to_add_test)
  labels_test = append(labels_test, rep(author_name_test, length(files_to_add_test)))
}


# Need a more clever regex to get better names here
test_docs = lapply(file_list_test, readerPlain) 
names(test_docs) = file_list_test
names(test_docs) = sub('.txt', '', names(test_docs))
my_corpus_test = Corpus(VectorSource(test_docs))

my_corpus_test = tm_map(my_corpus_test, content_transformer(tolower)) # make everything lowercase
my_corpus_test = tm_map(my_corpus_test, content_transformer(removeNumbers)) # remove numbers
my_corpus_test = tm_map(my_corpus_test, content_transformer(removePunctuation)) # remove punctuation
my_corpus_test = tm_map(my_corpus_test, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus_test = tm_map(my_corpus_test, content_transformer(removeWords), stopwords("SMART"))

DTM_test = DocumentTermMatrix(my_corpus_test)
DTM_test = removeSparseTerms(DTM_test, 0.975)

test_data = as.data.frame(as.matrix(DTM_test))

### RANDOM FOREST #### 

set.seed(1234)
rf.model <- randomForest(x=training,y=factor(labels_train),ntree=50)
rf.prediction = predict(rf.model,newdata=test_data)
predictions = table(rf.prediction,labels_train)
count = 0
for(i in 1:50){
  count = count + predictions[i,i]
}
