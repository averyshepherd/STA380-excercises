## The tm library and related plugins comprise R's most popular text-mining stack.
## See http://cran.r-project.org/web/packages/tm/vignettes/tm.pdf

library(NLP)

rm(list=ls())

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

# I've stored this function as a Github "gist" at:
# https://gist.github.com/jgscott/28d9d1287a0c3c1477e2113f6758d5ff

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


## apply to all of Simon Cowell's articles
## (probably not THE Simon Cowell: https://twitter.com/simoncowell)
## "globbing" = expanding wild cards in filename paths
train_file_list = Sys.glob('ReutersC50/C50train/*/*.txt')
train_data = lapply(train_file_list, readerPlain) 

test_file_list = Sys.glob('ReutersC50/C50test/*/*.txt')
test_data = lapply(test_file_list, readerPlain) 


extract_author <- function(x) {
  strsplit(x, "/")
}

data = as.data.frame(lapply(extract_author(train_file_list), function(x) x[length(x) - 1] ))
author <- t(df)
rownames(author)<-seq(1,2500)
# Clean up the file names
# no doubt the stringr library would be nicer here.
# this is just what I hacked together
mynames_train = train_file_list %>%
  { strsplit(., '/', fixed=TRUE) } %>%
  { lapply(., tail, n=2) } %>%
  { lapply(., paste0, collapse = '') } %>%
  unlist

mynames_test = test_file_list %>%

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

#mynames
names(train_data) = mynames_train
names(test_data)=mynames_test

## once you have documents in a vector, you 
## create a text mining 'corpus' with: 
documents_raw = Corpus(VectorSource(train_data))
documents_raw_test = Corpus(VectorSource(test_data))


## Some pre-processing/tokenization steps.
## tm_map just maps some function to every document in the corpus
my_documents = documents_raw %>%
  tm_map(content_transformer(tolower))  %>%             # make everything lowercase
  tm_map(content_transformer(removeNumbers)) %>%        # remove numbers
  tm_map(content_transformer(removePunctuation)) %>%    # remove punctuation
  tm_map(content_transformer(stripWhitespace))          # remove excess white-space


my_documents_test = documents_raw_test %>%
  tm_map(content_transformer(tolower))  %>%             # make everything lowercase
  tm_map(content_transformer(removeNumbers)) %>%        # remove numbers
  tm_map(content_transformer(removePunctuation)) %>%    # remove punctuation
  tm_map(content_transformer(stripWhitespace))    


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

my_documents_test = tm_map(my_documents_test, content_transformer(removeWords), stopwords("en"))
## create a doc-term-matrix from the corpus
DTM = DocumentTermMatrix(my_documents)
DTM_test = DocumentTermMatrix(my_documents_test)
 # some basic summary statistics

## You can inspect its entries...
#inspect(DTM_simon[1:10,1:20])

## ...find words with greater than a min count...
#findFreqTerms(DTM_simon, 50)

## ...or find words whose count correlates with a specified word.
# the top entries here look like they go with "genetic"
#findAssocs(DTM_simon, "genetic", .5)


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

DTM = removeSparseTerms(DTM, 0.95)
DTM_test = removeSparseTerms(DTM_test, 0.95)


# construct TF IDF weights -- might be useful if we wanted to use these
# as features in a predictive model
tfidf_train = weightTfIdf(DTM)
tfidf_test = weightTfIdf(DTM_test)

train.data <- data.frame(as.matrix(tfidf_train), stringsAsFactors=FALSE)

#Merge with author names
train.data <- merge(train.data,author,by =0)
train.data$V1 <- as.factor(train.data$V1)

test.data <- data.frame(as.matrix(tfidf_test), stringsAsFactors=FALSE)
#Merge with author names
test.data <- merge(test.data,author,by =0)


v1 <- c(names(train.data))
v2 <- c(names(test.data))
xc <- intersect(v1,v2)
new_test <- test.data %>% select(xc)
new_train<-train.data %>% select(xc)

library(randomForest)
set.seed(1234)
train_rf = randomForest(new_train$V1~., data=new_train, ntree=37, proximity=T)
table(predict(train_rf), train.data$V1)
train_rf
plot(train_rf)


testPred = predict(train_rf, newdata=new_test, type = 'class')
#table(testPred, test.data$V1)
CM = table(testPred, new_test$V1)
accuracy = (sum(diag(CM)))/sum(CM)
accuracy
####
# Dimensionality reduction
####

# Now PCA on term frequencies
X = as.matrix(tfidf_train)
X_test=as.matrix(tfidf_test)
summary(colSums(X))
summary(colSums(X_test))
scrub_cols = which(colSums(X) == 0)
scrub_cols_test = which(colSums(X_test) == 0)
X = X[,-scrub_cols]
X_test = X_test[,-scrub_cols_test]

pca_train = prcomp(X, scale=TRUE)
pca_test=predict(pca_train,newdata =as.data.frame(t(X_test)))



# looks like 15 or so summaries get us ~50% of the variation in over 1000 features
summary(pca_test) 

# Look at the loadings
pca_simon$rotation[order(abs(pca_simon$rotation[,1]),decreasing=TRUE),1][1:25]
pca_simon$rotation[order(abs(pca_simon$rotation[,2]),decreasing=TRUE),2][1:25]


## Look at the first two PCs..
# We've now turned each document into a single pair of numbers -- massive dimensionality reduction
pca_simon$x[,1:2]

plot(pca_simon$x[,1:2], xlab="PCA 1 direction", ylab="PCA 2 direction", bty="n",
     type='n')
text(pca_simon$x[,1:2], labels = 1:length(simon), cex=0.7)

# 46 and 48 are pretty close
# Both about Scottish Amicable
content(simon[[46]])
content(simon[[48]])

# 25 and 26 are pretty close
# Both about genetic testing
content(simon[[25]])

content(simon[[26]])

# 10 and 11 pretty close
# Both about Ladbroke's merger
content(simon[[10]])
content(simon[[11]])

# Conclusion: even just these two-number summaries still preserve a lot of information
