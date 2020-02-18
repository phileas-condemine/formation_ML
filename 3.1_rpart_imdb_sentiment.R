# https://www.kaggle.com/lakshmi25npathi/imdb-dataset-of-50k-movie-reviews/data
library(data.table)
library(text2vec)
library(rpart)
library(rpart.plot)
library(magrittr)
library(SnowballC)
imdb=fread("data/IMDB Dataset.csv")
imdb$id = 1:nrow(imdb)
stemmed_docs = pbapply::pblapply(strsplit(imdb$review," "),function(x)paste(wordStem(x),collapse=" "))
stemmed_docs = unlist(stemmed_docs)
prep_fun = tolower
tok_fun = word_tokenizer
it_train = itoken(stemmed_docs, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = imdb$id, 
                  progressbar = FALSE)
vocab = create_vocabulary(it_train)
pruned_vocab = prune_vocabulary(vocab, 
                                doc_proportion_max = 0.5,
                                doc_proportion_min = 0.002)
vocab1000 = pruned_vocab[sample(nrow(pruned_vocab),1000),]
vectorizer = vocab_vectorizer(vocab1000)
dtm_train = create_dtm(it_train, vectorizer)
dtm_train_dense = data.frame(as.matrix(dtm_train))
dtm_train_dense$target = imdb$sentiment
tree = rpart(data = dtm_train_dense,target~.)
rpart.plot(tree)
print(tree)

plot(tree)
text(tree)
