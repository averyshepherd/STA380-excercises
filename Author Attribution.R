## The tm library and related plugins comprise R's most popular text-mining stack.
## See http://cran.r-project.org/web/packages/tm/vignettes/tm.pdf

rm(list=ls())
library(NLP)
library(tm) 
library(tidyverse)
library(slam)
library(proxy)


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


### TEST DATA #####
author_dirs_test = Sys.glob('data/ReutersC50/C50test/*')

file_list_test = NULL
labels_test = NULL
for(author_test in author_dirs_test) {
  author_name_test = tail(strsplit(author_test,split="/")[[1]],1)
  files_to_add_test = Sys.glob(paste0(author_test, '/*.txt'))
  file_list_test = append(file_list_test, files_to_add_test)
  labels_test = append(labels_test, rep(author_name_test, length(files_to_add_test)-1))
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
DTM_test # some basic summary statistics

class(DTM_test)  # a special kind of sparse matrix format

inspect(DTM_test[1:10,1:20])
DTM_test = removeSparseTerms(DTM_test, 0.975)

test.data = as.data.frame(as.matrix(DTM_test), stringsAsFactors=FALSE)


###RANDOM FOREST###
library(randomForest)
set.seed(1234)
common_cols = intersect(names(train.data), names(test.data))
X_train =train.data[,c(common_cols)]
train_rf = randomForest(X_train, data=train.data, ntree=10, mtry=3)
table(predict(train_rf), X_train)
train_rf
plot(train_rf)


testPred = predict(train_rf, newdata=new_test, type = 'class')
#table(testPred, test.data$V1)
CM = table(testPred, new_test$V1)
accuracy = (sum(diag(CM)))/sum(CM)
accuracy

