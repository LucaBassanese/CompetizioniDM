# Benchmark
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
library(fasttime)
library(SuperLearner)
train <- read.csv("99.csv", stringsAsFactors=T)
test <- read.csv("100.csv", stringsAsFactors=F)
train$count = log1p(train$count)


train %>% 
  mutate(datetime = fastPOSIXct(datetime, "GMT")) %>% 
  mutate(hour = hour(datetime),
         month = month(datetime),
         year = year(datetime),
         wday = wday(datetime)) -> train

test %>% 
  mutate(datetime = fastPOSIXct(datetime, "GMT")) %>% 
  mutate(hour = hour(datetime),
         month = month(datetime),
         year = year(datetime),
         wday = wday(datetime)) -> test





target <- train$count
n <- nrow(train)
m <- nrow(test)
combi <- rbind(train[, -c(1, 10, 11, 12)], test[,-1])
combi$season <- as.factor(combi$season)
combi$holiday <- as.factor(combi$holiday)
combi$workingday <- as.factor(combi$workingday)
combi$weather <- as.factor(combi$weather)
combi$hour <- as.factor(combi$hour)
combi$month <- as.factor(combi$month)
combi$year <- as.factor(combi$year)
combi$wday <- as.factor(combi$wday)
train <- combi[1:n, ]
test <- combi[(n+1):(n+m),]
train$count <- target


#problema outlier minori di 16 di humidity
#soluzione 1#
train$humidity[train$humidity < 16] <- 105

# #soluzione 2#
# train <- train[train$humidity >= 16, ]

# #soluzione 3#
# umin16 <- ifelse(train$humidity < 16, 1, 0)
# train$humidity[train$humidity < 16] <- sample(54:56, 32, replace = T)
# train$umin16 <- umin16


combi <- combi[, -c( 5)] #temp correlata con atemp, elimino temp

trainno <- combi[1:n,]
testno <- combi[(n+1):(n+m),]
traincaret <- trainno
traincaret$count <- target

############superlearner###########
#guida al pacchetto SuperLearner: https://cran.r-project.org/web/packages/SuperLearner/vignettes/Guide-to-SuperLearner.html
x <- trainno
y <- target

model <- SuperLearner(y,
                      x,
                      family =gaussian(),
                      SL.library = list( "SL.xgboost",
                                         "SL.gbm",
                                         "SL.randomForest", 
                                         "SL.ranger" ))

previsioni <- predict.SuperLearner(model, testno)
previsioni <- previsioni$pred
previsioni <- expm1(previsioni)
previsioni <- as.numeric(previsioni)
write.table(file="bikefinale.txt", previsioni, row.names = FALSE, col.names = FALSE)