library(caret)

setwd("C:/Users/rzij/Documents/Data Science Course/Course Project Machine Learning")


rm(list = ls())
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")

# Clear up for meta data, nearZeroVariables and Columns that contain mostly NA values.
training <- training[,-c(1:7)]
training <- training[,-c(nearZeroVar(training))]
training <- training[, apply(training, 2, function(x) !any(is.na(x)))]

# Repeat for the test data
testing <- testing[,-c(1:7)]
testing <- testing[,-c(nearZeroVar(testing))]
testing <- testing[, apply(testing, 2, function(x) !any(is.na(x)))]

# Split training set
split <- createDataPartition(training$classe, p = 0.7, list = FALSE)
training1 <- training[split,]
training2 <- training[-split,]

# Fit three predict models based on training1, test on training2, compare accuracy.
trainctrl <- trainControl(verboseIter = TRUE)

gbmModel <- train(classe ~ ., data = training1, method = "gbm", trControl = trainctrl)
gbmPredict <- confusionMatrix(predict(gbmModel, training2), training2$classe)
gbmPredict$overall[1]

rfModel <- train(classe ~ ., data = training1, method = "rf", trControl = trainctrl)
rfPredict <- confusionMatrix(predict(rfModel, training2), training2$classe)
rfPredict$overall[1]

rpartModel <- train(classe ~ ., data = training1, method = "rpart", trControl = trainctrl)
rpartPredict <- confusionMatrix(predict(rpartModel, training2), training2$classe)
rpartPredict$overall[1]

# Apply the best model on the test set
rfPredictTest <- predict(rfModel, testing)
submission <- data.frame(id = testing$problem_id, prediction = rfPredictTest)
submission
