## The tm library and related plugins comprise R's most popular text-mining stack.
## See http://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
library(NLP)
library(tm) 
library(tidyverse)
library(slam)
library(proxy)

rm(list=ls())
## tm has many "reader" functions.  Each one has
## arguments elem, language, id
## (see ?readPlain, ?readPDF, ?readXML, etc)
## This wraps another function around readPlain to read
## plain text documents in English.
readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }


### File not reading #####
author_dirs = Sys.glob('/data/ReutersC50/C50train/*/*.txt')
file_list = NULL
labels = NULL
for(author in author_dirs) {
  author_name = substring(author, first=29)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}

# # 
# # ## "globbing" = expanding wild cards in filename paths
# author_dirs = Sys.glob('data/ReutersC50/C50train/*/*.txt')
# reuters = lapply(file_list, readerPlain)
# 
# # # The file names are ugly...
# # file_list

# Clean up the file names
# no doubt the stringr library would be nicer here.
# this is just what I hacked together
mynames = file_list %>%
  { strsplit(., '/', fixed=TRUE) } %>%
  { lapply(., tail, n=2) } %>%
  { lapply(., paste0, collapse = '') } %>%
  unlist

# Rename the articles
mynames
names(reuters) = mynames

## once you have documents in a vector, you 
## create a text mining 'corpus' with: 
documents_raw = Corpus(VectorSource(reuters))

## Some pre-processing/tokenization steps.
## tm_map just maps some function to every document in the corpus
my_documents = documents_raw %>%
  tm_map(content_transformer(tolower))  %>%             # make everything lowercase
  tm_map(content_transformer(removeNumbers)) %>%        # remove numbers
  tm_map(content_transformer(removePunctuation)) %>%    # remove punctuation
  tm_map(content_transformer(stripWhitespace))          # remove excess white-space

## Remove stopwords.  Always be careful with this: one person's trash is another one's treasure.
# 2 example built-in sets of stop words
stopwords("en")
stopwords("SMART")
?stopwords
# let's just use the "basic English" stop words
my_documents = tm_map(my_documents, content_transformer(removeWords), stopwords("en"))

## create a doc-term-matrix from the corpus
DTM_reuters=DocumentTermMatrix(my_documents)
DTM_reuters # some basic summary statistics

## You can inspect its entries...
inspect(DTM_reuters[1:10,1:20])

## ...find words with greater than a min count...
findFreqTerms(DTM_reuters, 50)

## ...or find words whose count correlates with a specified word.
# the top entries here look like they go with "genetic"
findAssocs(DTM_reuters, "genetic", .5)

## Finally, let's drop those terms that only occur in one or two documents
## This is a common step: the noise of the "long tail" (rare terms)
## can be huge, and there is nothing to learn if a term occured once.
## Below removes those terms that have count 0 in >95% of docs.  
## Probably a bit stringent here... but only 50 docs!
DTM_reuters = removeSparseTerms(DTM_reuters, 0.95)
DTM_reuters # now ~ 1000 terms (versus ~3000 before)

# construct TF IDF weights -- might be useful if we wanted to use these
# as features in a predictive model
tfidf_reuters = weightTfIdf(DTM_reuters)

