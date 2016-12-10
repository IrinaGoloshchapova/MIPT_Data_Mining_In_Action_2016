# rm(list = ls())
# setwd('~/Programming/Python/Learning/MIPT/Homeworks/Project_Otzyvy')
# setwd('D:/Irina/Project_Otzyvy')
# load('Classification.RData')

library(purrr)
library(ggplot2)
library(text2vec)
library(tidyr)
library(dplyr)
library(lubridate)
library(caret)
library(data.table)
require(stringr)
require(tm)
library(parallel)
library(doParallel)
library(xgboost)
library(readr)
library(stringr)
library(car)

# functions
## Еще можно было бы вычищать все цифры!!!
# clean_text <- function(doc) {
#   # задаем список служебных слов
#   stopwords <- c("в", "без", "до", "из", "к", "на", "по", "о", "от", "перед", "при", "через", "за", "над", "об", "под", "про", "для", "вблизи", "вглубь", "вдоль", "возле", "около", "вокруг", "впереди", "после", "посредством", "в роли", "в зависимости от", "путём", "насчёт", "по поводу", "ввиду", "по случаю", "в течение", "благодаря", "несмотря на", "спустя", "с ", "из-под", "из-за", "по-над", "в отличие от", "в связи", "как", "словно", "так как", "для того чтобы", "тоже", "зато", "чтобы", "также", "потому что", "и ", "а ", "что", "или", "но", "однако", "когда", "лишь", "едва", "где", "куда", "откуда", "столько", "настолько", "так", "до такой степени", "до того", "такой", "как будто", "будто", "точно", "как бы","если", "если бы", "коли", "ежели", "несмотря на то", "хотя", "хоть", "пускай", "дабы", "с тем чтобы", "так что", "ли", "не", "какой")
#   doc <- enc2utf8(doc)
#   doc <- tolower(doc)
#   doc <- removeWords(doc, stopwords)                # удаляем служебные слова, плохо работает с кириллицей в CP1251
#   doc <- removePunctuation(doc)                     # удаляем знаки пунктуации, плохо работает с кириллицей в CP1251
#   doc <- str_replace_all(doc, "\\s+", " ")          # удаляем лишние пробелы
#   doc <- str_trim(doc, side = "both")               # удаляем пробелы в начале и в конце строки
#   enc2native(doc)
# }
# 
# stem_text <- function(x) {
#   res <- system("C:/mystem-3.0-win7-64bit/mystem -cl -e cp1251", intern = TRUE, input = x)
#   res <- paste(res, sep = '', collapse = '')
#   res <- gsub("[{}]", "", res)
#   res <- gsub("(\\|[^ ]+)", "", res)
#   res <- gsub("\\?", "", res)
#   res <- gsub("\\s+", " ", res)
#   res
# }

# #loading objects
# load('data.rda')
# 
# #preprocessing
# data <- data %>% mutate(rating = as.factor(rating), text = gsub('\"', '', text, fixed = TRUE))
# data <- data %>% mutate(text = gsub('<.+>|', '', clean_text(gsub('<.+>|', '', text))), title = clean_text(title))
# 
# # stemming
# data$text <- sapply(seq_len(length(data$text)), function(i) stem_text(data$text[i])) # долго
# data$title <- sapply(seq_len(length(data$title)), function(i) stem_text(data$title[i])) # долго
# 
# # for (i in seq_len(length(data$text))) {
# #   cat('\n Row', i)
# #   data[i, ]$text = stem_text(data[i, ]$text)
# # }

load('data_cleaned_stemmed.rda')

# cleaning NA data
data <- data[complete.cases(data), ]

# forming train data
data <- data %>% mutate(rating = as.factor(rating))
inTrain <- createDataPartition(y = data$rating, p = 0.8, list = FALSE)

train <- data[inTrain, ]
test <- data[-inTrain, ]

# preprocessing
# train <- select(train, id, text, rating)
train <- train %>% mutate(text = enc2native(text), title = enc2native(title))

setDT(train)
setkey(train, id)
set.seed(23)

setDT(test)
setkey(test, id)


# define preprocessing function and tokenization function
# prep_fun <- tolower
tok_fun <- word_tokenizer
prep_fun <- identity

it_train <- itoken(train$text, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun, 
                   ids = train$id, 
                   progressbar = TRUE)
vocab <- create_vocabulary(it_train, ngram = c(1L, 2L))

# чтобы увидеть слова
# iconv(vocab_min$vocab$terms[1:92], to = 'cp1251')

# # prune vocabulary
vocab_min = vocab %>% prune_vocabulary(term_count_min = 50, doc_proportion_max = 0.5, max_number_of_terms = 1000)

# construct a document-term matrix.
bigram_vectorizer = vocab_vectorizer(vocab_min)
dtm_train <- create_dtm(it_train, bigram_vectorizer)

dim(dtm_train)
# a <- as(dtm_train, 'matrix')

# define tfidf model
tfidf = TfIdf$new()
# fit model to train data and transform train data with fitted model
dtm_train_tfidf = fit_transform(dtm_train, tfidf)

# test preparation
it_test = test$text %>% 
  prep_fun %>% 
  tok_fun %>% 
  itoken(ids = test$id, 
         # turn off progressbar because it won't look nice in rmd
         progressbar = TRUE)

dtm_test <- create_dtm(it_test, bigram_vectorizer)
dtm_test_tfidf <- create_dtm(it_test, bigram_vectorizer) %>% 
  transform(tfidf)

train$rating <- factor(train$rating, labels = c('r1', 'r2', 'r3', 'r4', 'r5'))
test$rating <- factor(test$rating, labels = c('r1', 'r2', 'r3', 'r4', 'r5'))

# #run model in parallel
# cl <- makeCluster(detectCores())
# registerDoParallel(cl)
# 
# # Train and Tune the SVM
# svm.tune <- train(x = dtm_train,
#                   y = train$rating,
#                   method = "svmRadial",   # Radial kernel
#                   tuneLength = 10,					# 9 values of the cost function
#                   preProcess = NULL,  # Center and scale data
#                   metric = "accuracy", 
#                   trControl = trainControl(method = "cv", allowParallel = TRUE))
# 
# stopCluster(cl)

# library(e1071)
# library(parallelSVM)
# # svm_model <- svm(x = dtm_train, y = train$rating, kernel = 'linear')
# 
# # run model in parallel
# cl <- makeCluster(detectCores() - 1)
# registerDoParallel(cl)
# 
# model <- parallelSVM(x = as(dtm_train, 'matrix'), y = train$rating, kernel = 'linear',  numberCores = 3)
# 
# stopCluster(cl)
datlabels <- as.numeric(as.factor(train$rating)) - 1
labels_corresponds <- table(train$rating, datlabels)
test_datlabels <- as.numeric(as.factor(test$rating)) - 1
table(test$rating, test_datlabels)

model_xgboost <- xgboost(data = dtm_train, label = datlabels, nrounds = 1000, num_class = 5, objective = "multi:softmax")

# dtm_test <- create_dtm(it_test, bigram_vectorizer)
pred <- predict(model_xgboost, dtm_test)
confusionMatrix(pred, test_datlabels)

model_xgboost_tfidf <- xgboost(data = dtm_train_tfidf, label = datlabels, nrounds = 1000, num_class = 5, objective = "multi:softmax")
pred_tfidf <- predict(model_xgboost_tfidf, dtm_test_tfidf)
confusionMatrix(pred_tfidf, test_datlabels)

# ## ------------------------
# # stats - importance
# ## ------------------------
# stats <- xgb.dump(model_xgboost_tfidf, with.stats = T)
# feature_names <- vocab_min$vocab$terms
# 
# importance_matrix <- xgb.importance(feature_names, model = model_xgboost_tfidf)
# xgb.plot.importance(importance_matrix[1:10,])
# xgb.plot.tree(feature_names, model = model_xgboost_tfidf)

## ------------------------
# xgboost tuning
## ------------------------
tfidf_cv <- xgb.cv(data = dtm_train_tfidf, label = datlabels, nfold = 5, nrounds = 1000, num_class = 5, objective = "multi:softmax",
                   maximize = FALSE,
                   early.stop.round = 3)

# Mean merror (exact matching error) - stdev
bestRound <- which.max(as.matrix(tfidf_cv)[, 3] - as.matrix(tfidf_cv)[, 4])
tfidf_cv[bestRound, ]
bestRound <- 91

model_xgboost_tfidf <- xgboost(data = dtm_train_tfidf, label = datlabels, nrounds = 100, num_class = 5, objective = "multi:softmax", maximize = FALSE)
pred_tfidf <- predict(model_xgboost_tfidf, dtm_test_tfidf, ntreelimit = 1) # bestRound
confusionMatrix(pred_tfidf, test_datlabels)

## ------------- small eta ----------------------
tfidf_cv <- xgb.cv(data = dtm_train_tfidf, label = datlabels, nfold = 5, eta = 0.1, nrounds = 1000, num_class = 5, objective = "multi:softmax",
                   maximize = FALSE,
                   early.stop.round = 3)
bestRound <- 185
model_xgboost_tfidf <- xgboost(data = dtm_train_tfidf, label = datlabels, eta = 0.1, nrounds = 200, num_class = 5, objective = "multi:softmax", maximize = FALSE)
pred_tfidf <- predict(model_xgboost_tfidf, dtm_test_tfidf, ntreelimit = bestRound) 
confusionMatrix(pred_tfidf, test_datlabels)

# save(model_xgboost_tfidf, file = 'model_xgboost_tfidf.rda')
# save.image('Classification.RData')
# for application
# save.image('App_Data.RData')