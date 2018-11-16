setwd("C:\\Users\\banab\\Desktop\\Dataset")
rm(list = ls())

library(ggridges)
library(pROC)
library(rpart)
library(ggplot2)
library(ade4)
library(caret)
library(dplyr)
library(VIM)
library(ggcorrplot)
library(lsr)
library(randomForestSRC) 
library(ggRandomForests)
library(gridExtra)
library(SuperLearner)
train <- read.csv("101.csv", stringsAsFactors=T)
test <- read.csv("102.csv",  stringsAsFactors=T)

test$Class = NA

n = nrow(train)
m = nrow(test)
combip = rbind(train,test)
train = combip[1:n,]
test = combip[(n+1):(n+m),]
combi <- combip[, -31]

#eliminazione quantitative e qualitative ininfluenti:
#age, height, last_online, figuring, #lol, animals, poltica, #health, adults, native, nyc, singning

combi <- combi[, -c( 1, 7, 9, 21:28, 48, 62, 66, 67, 72, 76, 80, 88, 89)]


#livelli

levels(combi$sign) <- list(Pazzi=c("aquarius", "aries", "cancer", "capricorn", "gemini", "leo", "libra",
                                   "pisces", "sagittarius", "scorpio", "taurus", "virgo"),
                           Normali = c("sign_missing") )
#chi crede all'oroscopo è un pazzo

#dummy

dummy <- function(df) {  
  
  
  NUM <- function(dataframe)dataframe[,sapply(dataframe,is.numeric)]
  FAC <- function(dataframe)dataframe[,sapply(dataframe,is.factor)]
  
  require(ade4)
  if (is.null(ncol(NUM(df)))) {
    DF <- data.frame(NUM(df), acm.disjonctif(FAC(df)))
    names(DF)[1] <- colnames(df)[which(sapply(df, is.numeric))]
  } else {
    DF <- data.frame(NUM(df), acm.disjonctif(FAC(df)))
  }
  return(DF)
} 
#Formula per convertire in dummies presa da Stack Overflow: https://stackoverflow.com/questions/5048638/automatically-expanding-an-r-factor-into-a-collection-of-1-0-indicator-variables

combi <- dummy(combi)

#elimino i livelli inutili:
#kosher e mostly kosher ed strickly kosher ed mostly halal,  inc500000, dislikes_dogs, dislikes_dogs_and_has_cats,
#benicia, el_sobrante, green_brae, hercules, pinole. Hanno ridotta numerosità per essere significativi.

combi <- combi[, -c(70, 72, 73, 84, 135, 160, 162, 200, 207, 211, 214, 229)]



minimo <- min(combi[, 3])
massimo <- max(combi[, 3])
combi[, 3] <- scale(combi[, 3], center = minimo, scale = massimo-minimo)
train <- as.matrix(combi[1:n,])
test <- as.matrix(combi[n+1:m, ])
target <- combip$Class[1:n]
train_up <- upSample(x = train, y=target, yname="class")
train <- as.data.frame(train)
target <- as.numeric(target) -1
casuale <- sample(1:nrow(train_up))
train_up <- train_up[casuale, ]
target_up <- train_up$class
target_up <- as.numeric(target_up)-1
test <- as.data.frame(test)
traincaret <- train
traincaret$class <- combip$Class[1:n]

##########superlearner#####
set.seed(345)
model <- SuperLearner(target,
                      train,
                      family =binomial(),
                      method = "method.AUC",
                      SL.library = list(  "SL.glmnet",  "SL.biglasso", "SL.gbm",  "SL.randomForest" , "SL.ranger")
)

previsionisup <- predict.SuperLearner(model, test)
previsionisup <- previsionisup$pred
previsionisup <- as.numeric(previsionisup)


##########gbm############
fitControl <- trainControl(
  method = "repeatedcv",
  number = 4,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
  
)

gridcaret <-  expand.grid( n.trees = 100 ,
                           interaction.depth = 3,
                           shrinkage = 0.1,
                           n.minobsinnode= 10
)

modello <- train(class~ ., data = traincaret, 
                 method = 'gbm', 
                 trControl = fitControl,
                 verbose = FALSE, 
                 tuneGrid = gridcaret ,
                 metric = "ROC"
)

previsioni <- predict(modello, test, type = "prob")[, "stem"]


##########ranger########
set.seed(123)
fitControl <- trainControl(
  method = "repeatedcv",
  number = 4,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
  
)

gridcaret <-  expand.grid( mtry = 3,
                           splitrule = "extratrees" ,
                           min.node.size = 5
)

modello <- train(class~ ., data = train_up, 
                 method = 'ranger', 
                 trControl = fitControl,
                 verbose = FALSE, 
                 tuneGrid = gridcaret ,
                 metric = "ROC"
                 
)

previsionirangov <- predict(modello, test, type = "prob")[, "stem"]


set.seed(2388)
fitControl <- trainControl(
  method = "repeatedcv",
  number = 4,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
  
)

gridcaret <-  expand.grid( mtry = 3,
                           splitrule = "extratrees" ,
                           min.node.size = 5
)

modello <- train(class~ ., data = traincaret, 
                 method = 'ranger', 
                 trControl = fitControl,
                 verbose = FALSE, 
                 tuneGrid = gridcaret ,
                 metric = "ROC"
                 
)

previsionirang <- predict(modello, test, type = "prob")[, "stem"]
#########xgboost#####
set.seed(333)
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
  
)
gridcaret <-  expand.grid( nrounds = 100,
                           max_depth = 2 ,
                           eta = 0.2 ,
                           gamma = 0,
                           subsample = 0.9, 
                           colsample_bytree = 0.5,
                           rate_drop = 0.4,
                           skip_drop =0.9,
                           min_child_weight =0
)

modello <- train(class~ ., data = train_up, 
                 method = 'xgbDART', 
                 trControl = fitControl,
                 verbose = FALSE, 
                 tuneGrid = gridcaret ,
                 metric = "ROC"
)

previsionixgb <- predict(modello, test, type = "prob")[, "stem"]
write.table(file="mille.txt", previsioni, row.names = FALSE, col.names = FALSE)







#######unione modelli#########
prevfinale2 <- 0.4 * previsionisup + 0.2 * previsionirangov + 0.2 * previsionirang + 0.1* previsionixgb + 0.1 * previsioni
write.table(file="okfinale.txt", prevfinale2, row.names = FALSE, col.names = FALSE)