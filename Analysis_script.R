library(data.table)
library(caret)
library(ggplot2)
library(parallel)
library(doParallel)
library(visdat)
library(purrr)
library(dplyr)
library(printr)
library(corrplot)
library(kableExtra)
library(gbm)
library(e1071)
library(randomForest)
library(forecast)

required_libraries <- c("data.table", "caret", "ggplot2", "parallel","doParallel",
                        "visdat", "purrr", "dplyr", "printr","corrplot",
                        "kableExtra", "gbm", "e1071", "randomForest","forecast")
library_check <- sapply(required_libraries, require, character.only = TRUE, 
                        quietly = TRUE)


if (!file.exists("./data")) {
  dir.create("./data")
}


trainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

str(trainData)

# subset the datasets
trainData <- trainData[, -c(1:7)]
testData <- testData[, -c(1:7)]

# dimensions of the data
rbind(training = dim(trainData),
      testing = dim(testData))

set.seed(2023)
inTrain <- createDataPartition(y = trainData$classe, p = 0.7, list = F)
training <- trainData[inTrain, ]
testing <- trainData[-inTrain, ]


nzv <- nearZeroVar(trainData, saveMetrics = T)
keepFeat <- row.names(nzv[nzv$nzv == FALSE, ])
training <- training[, keepFeat]

training <- training[, colSums(is.na(training)) == 0]
dim(training)


corr_data <- select_if(training, is.numeric)
corrplot(
  cor(corr_data),
  method = "color",
  tl.pos = "n",
  insig = "blank"
)


modCtl <- trainControl(method = 'cv', number = 5)

set.seed(2024)
#modRf <- train(classe ~. , data = training, method = 'rf', trControl = modCtl)
#saveRDS(modRf,file="modRf.rds")

modRf <- readRDS(file = "modRf.rds")
modRf

modRf$finalModel

plot(modRf$finalModel)

importance <- varImp(modRf, scale = FALSE)
plot(importance, top=10)

pred.rf <- predict(modRf, testing)
testing$classe <- factor(testing$classe, levels = 
                           c("A", "B", "C", "D", "E"))
confusionMatrix(pred.rf, testing$classe)$table

confusionMatrix(pred.rf, testing$classe)$overall[1]

#modGbm <- train(classe ~., data = training, method = 'gbm', trControl = modCtl, verbose = F)
#saveRDS(modGbm ,file="modGbm .rds")

modGbm  <- readRDS(file = "modGbm .rds")
modGbm 

modGbm$finalModel

predGbm <- predict(modGbm, newdata = testing)
confusionMatrix(predGbm, testing$classe)$table

confusionMatrix(predGbm, testing$classe)$overall[1]

predRfTest <- predict(modRf, newdata = testData)
predRfTest

predGbmTest <- predict(modGbm, newdata = testData)
predGbmTest

table(predRfTest, predGbmTest)










