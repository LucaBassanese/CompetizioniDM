setwd("C:\\Users\\banab\\Desktop\\Dataset")
rm(list = ls())


library(ggplot2)
library(ade4)
library(caret)
library(dplyr)
library(VIM)
library(ggcorrplot)
library(lsr)
library(randomForestSRC) 
library(ggRandomForests)

train <- read.csv("60.csv", stringsAsFactors=T)
test <- read.csv("61.csv", stringsAsFactors=T)

n <- nrow(train)
m <- nrow(test)
train_targets <- train[, 82]

#####Missing####
train<- train[,-82]
combi <- rbind(train, test)

combi$MS.SubClass <- as.factor(combi$MS.SubClass)



addlevels <- function(x, nuovolivello)  return(factor(x, levels=c(levels(x), nuovolivello)))

combi$Pool.QC <- addlevels(combi$Pool.QC, "No pool")
combi$Pool.QC[is.na(combi$Pool.QC)] <- "No pool"

combi$Misc.Feature <- addlevels(combi$Misc.Feature, "None")
combi$Misc.Feature[is.na(combi$Misc.Feature)]<- "None"

combi$Alley <- addlevels(combi$Alley, "No alley acces")
combi$Alley[is.na(combi$Alley)]<- "No alley acces"

combi$Fence <- addlevels(combi$Fence, "No Fence")
combi$Fence[is.na(combi$Fence)]<- "No Fence"

combi$Fireplace.Qu <- addlevels(combi$Fireplace.Qu, "No Fireplace")
combi$Fireplace.Qu[is.na(combi$Fireplace.Qu)]<- "No Fireplace"

combi$Garage.Yr.Blt[is.na(combi$Garage.Yr.Blt)] <- 1900 # ai senza garage metto come se fosse estremamente antico


combi$Garage.Finish <- addlevels(combi$Garage.Finish, "No Garage")
combi$Garage.Finish[is.na(combi$Garage.Finish)]<- "No Garage"

combi$Garage.Qual <- addlevels(combi$Garage.Qual, "No Garage")
combi$Garage.Qual[is.na(combi$Garage.Qual)]<- "No Garage"

combi$Garage.Cond <- addlevels(combi$Garage.Cond, "No Garage")
combi$Garage.Cond[is.na(combi$Garage.Cond)]<- "No Garage"

combi$Garage.Type <- addlevels(combi$Garage.Type, "No Garage")
combi$Garage.Type[is.na(combi$Garage.Type)]<- "No Garage"

combi$Bsmt.Qual <- addlevels(combi$Bsmt.Qual, "No Basement")
combi$Bsmt.Qual[is.na(combi$Bsmt.Qual)]<- "No Basement"

combi$Bsmt.Cond <- addlevels(combi$Bsmt.Cond, "No Basement")
combi$Bsmt.Cond[is.na(combi$Bsmt.Cond)]<- "No Basement"


combi$Bsmt.Exposure <- addlevels(combi$Bsmt.Exposure, "No Basement")
combi$Bsmt.Exposure[is.na(combi$Bsmt.Exposure)]<- "No Basement"

combi$BsmtFin.Type.1 <- addlevels(combi$BsmtFin.Type.1, "No Basement")
combi$BsmtFin.Type.1[is.na(combi$BsmtFin.Type.1)]<- "No Basement"

combi$BsmtFin.Type.2 <- addlevels(combi$BsmtFin.Type.2, "No Basement")
combi$BsmtFin.Type.2[is.na(combi$BsmtFin.Type.2)]<- "No Basement"


for (i in 1:nrow(combi)){
  if(is.na(combi$Lot.Frontage[i])){
    combi$Lot.Frontage[i] <- as.integer(median(combi$Lot.Frontage[combi$Neighborhood==combi$Neighborhood[i]], na.rm=TRUE)) 
  }
}
#Formula ed idea prese da lavoro precedente di Erik Bruin : https://www.kaggleusercontent.com/kf/3560470/eyJhbGciOiJkaXIiLCJlbmMiOiJBMTI4Q0JDLUhTMjU2In0..aya1y1nQDrsKzb5lWsLp-g.1dJaib6jidkwdvC_dIHyXc5ahiCcY4Paiaf1LP7OjbgQ0ZwmAmTTtObZD9lDQDPRuRWzwS14dyQinTyada6uIUa0oMVtiLqOHvs-eho4KLZoiEmjKkAAX6va3HatwJX4vMix2mYmxSWWCVljidEgqpMjybX0UKnWiqirBe6-_aa9aSp_Ln03UednacGda8XT.dR48an8bUFEOyEFZ2A5oNg/__results__.html#imputing-missing-data
#mi è sembrata ragionevole la sua idea:  "Linear feet of street connected to property: The most reasonable imputation seems to take the median per neigborhood" - Erik Briun
combi$Mas.Vnr.Area[is.na(combi$Mas.Vnr.Area)] <- 0

combi$Bsmt.Full.Bath[is.na(combi$Bsmt.Full.Bath)] <- 0
combi$Bsmt.Half.Bath[is.na(combi$Bsmt.Half.Bath)] <- 0




combi$Garage.Area[is.na(combi$Garage.Area)]<-0
combi$Garage.Cars[is.na(combi$Garage.Cars)]<-0

combi$BsmtFin.SF.2[is.na(combi$BsmtFin.SF.2)]<-0
combi$BsmtFin.SF.1[is.na(combi$BsmtFin.SF.1)]<-0
combi$Bsmt.Unf.SF[is.na(combi$Bsmt.Unf.SF)]<-0
combi$Total.Bsmt.SF[is.na(combi$Total.Bsmt.SF)]<-0

combi[which(is.na(combi$Lot.Frontage)),]
combi$Lot.Frontage[is.na(combi$Lot.Frontage)]<-median(combi$Lot.Frontage, na.rm = T)

combi <- combi[,-1]


levels(combi$Condition.2) <- list(POS=c("PosA", "PosN"),
                                  AdjStr = c("Feedr", "Artery"),
                                  Norm = "Norm",
                                  RR = c("RRAe", "RRNn", "RRAn"))

levels(combi$Pool.QC) <- list(Yes = c("Fa", "TA", "Gd", "Ex"),
                              No = c("No pool"))

levels(combi$Sale.Type) <- list(Con = c("ConLI", "Con"),
                                COD = "COD", ConLD = "ConLD",
                                ConLw = "ConLw", CWD = "CWD",
                                New = "New", Oth = "Oth",
                                WD= "WD ", VWD = "VWD")


eliminazione <- c("Utilities", "Land.Slope", "Misc.Feature", "Fence", "Overall.Cond", "BsmtFin.SF.2", 
                  "Low.Qual.Fin.SF", "Bsmt.Half.Bath", "Kitchen.AbvGr", "Enclosed.Porch", "X3Ssn.Porch", 
                  "Screen.Porch", "Pool.Area",
                  "Misc.Val", "Mo.Sold", "Yr.Sold", "Bldg.Type", "House.Style")

pulito <- combi[, !(names(combi) %in% eliminazione)]
train <- pulito[1:n, ]
test <- pulito[(n+1):(n+m),]
traintot <- train
traintot$Sale.price <- log(train_targets)
traintot <- traintot[-1112, ] #outlier casa gigante ma poco costosa


###############Dummy#############
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


uniti <- dummy(pulito)

uniti <- uniti[, -c(76, 93, 106, 117, 118, 119, 120, 124, 127, 134, 135, 136, 141, 146, 153, 159, 167, 174, 178, 180, 182, 185, 187, 189, 201, 203, 211, 215, 220, 224, 229, 234, 240, 241, 259, 263, 265, 266, 268, 273, 283, 288)]
#eliminazione variabili dummies con numerosità ridotta o nulla tra test e train
train <- uniti[1:n, ]
test <- uniti[(n+1):(n+m),]
train$Sale.price <- log(train_targets)
train <- train[-1112, ]

rid <- train$Sale.price > 13 | train$Sale.price < 10.4 #elimino righe outlier di Sale.Price
trainrid <- train[rid==F,]

eliminomi <- c("MS.SubClass.75",         "MS.SubClass.90",         "MS.Zoning.C..all." ,     "Alley.No.alley.acces"  , "Lot.Shape.IR3" ,        
               "Land.Contour.Low",       "Lot.Config.CulDSac",     "Neighborhood.Blueste",   "Neighborhood.IDOTRR",    "Neighborhood.Mitchel" , 
               "Neighborhood.StoneBr" ,  "Neighborhood.SWISU" ,    "Condition.1.PosN"  ,     "Condition.1.RRAn" ,      "Condition.2.AdjStr"  , 
               "Roof.Style.Gambrel"  ,   "Roof.Matl.Tar.Grv",      "Exterior.1st.BrkComm",   "Exterior.1st.Stucco",    "Exterior.1st.WdShing"  ,
               "Exterior.2nd.HdBoard",   "Exterior.2nd.Plywood",   "Exterior.2nd.Stone" ,    "Exterior.2nd.Stucco",    "Exterior.2nd.Wd.Sdng" , 
               "Exter.Cond.TA",          "Foundation.Slab" ,       "Bsmt.Qual.Fa"  ,         "Bsmt.Qual.No.Basement",  "Bsmt.Cond.Fa" ,         
               "BsmtFin.Type.1.Rec"   ,  "BsmtFin.Type.2.BLQ" ,    "BsmtFin.Type.2.GLQ",     "BsmtFin.Type.2.LwQ",     "BsmtFin.Type.2.Rec",    
               "Heating.GasA",           "Heating.GasW",           "Heating.QC.Fa",          "Functional.Maj2",        "Functional.Min2",       
               "Fireplace.Qu.Po",        "Garage.Type.Basment",    "Garage.Cond.Fa",         "Garage.Cond.Gd",         "Paved.Drive.P" ,        
               "Sale.Condition.AdjLand", "Sale.Condition.Family" )
#variabili non importanti secondo importanza di random forest
eli <-names(uniti) %in% eliminomi
unitiel <- uniti[, eli==F]
trainel <- unitiel[1:n, ]
testel <- unitiel[(n+1):(n+m),]
trainel$Sale.price <- log(train_targets)
trainel <- trainel[-1112, ]



trainridel <- trainel[rid==F,]
ultimariduzione <- names(trainridel) 
ultimariduzione[c(45:48, 55:58, 94:96, 123:125, 87:88, 52:54, 193:194, 195:201, 184:187, 157:160 )]
trainfin <- trainridel[,-c(45:48, 55:58, 94:96, 123:125, 87:88, 52:54, 193:194, 195:201, 184:187, 157:160 ) ]
testfin <- testel[, -c(45:48, 55:58, 94:96, 123:125, 87:88, 52:54, 193:194, 195:201, 184:187, 157:160 )]
#variabili non importanti secondo importanza di bagging










######gbm#########
set.seed(444)
fitControl <- trainControl(
  method = "repeatedcv",
  number = 7,
  repeats = 7
  
)

gridcaret <-  expand.grid( n.trees = 200 ,
                           interaction.depth = 10,
                           shrinkage = 0.08,
                           n.minobsinnode= 5
)

modello <- train(Sale.price ~ ., data = trainfin, 
                 method = 'gbm', 
                 trControl = fitControl, 
                 preProcess = c('center', 'scale'),
                 verbose = FALSE, 
                 tuneGrid = gridcaret ,
                 metric = "RMSE"
)

previsionigbm <- predict(modello, testfin)




######bnn########
set.seed(111)
fitControl <- trainControl(
  method = "repeatedcv",
  number = 7,
  repeats = 7
)

gridcaret <-  expand.grid( neurons = 1
)

modello <- train(Sale.price ~ ., data = trainfin, 
                 method = 'brnn', 
                 trControl = fitControl, 
                 preProcess = c('center', 'scale'),
                 verbose = FALSE, 
                 tuneGrid = gridcaret ,
                 metric = "RMSE"
)
previsionibnn <- predict(modello, testfin)



############xgboost##############

fitControl <- trainControl(
  method = "repeatedcv",
  number = 7,
  repeats = 7
)

xgboostgrid <-  expand.grid( nrounds = 200,
                             max_depth = 6 ,
                             eta = 0.2 ,
                             gamma = 0,
                             subsample = 0.9, 
                             colsample_bytree = 0.6,
                             rate_drop = 0,
                             skip_drop =0.9,
                             min_child_weight =0
)

xgboost <- train(Sale.price ~ ., data = trainfin, 
                 method = "xgbDART", 
                 trControl = fitControl, 
                 preProcess = c('center', 'scale'),
                 verbose = FALSE, 
                 tuneGrid = xgboostgrid,
                 metric = "RMSE"
)
previsionixg <- predict(xgboost, testfin)


####unione modelli differenti####
previparz <- 0.6*previsionigbm + 0.3 * previsionibnn + 0.1 * previsionixg
previparz <- exp(previparz)
write.table(file="amesparz.txt", previparz, row.names = FALSE, col.names = FALSE)

