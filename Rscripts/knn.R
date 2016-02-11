rm(list=ls())

setwd("C:/Docs/Dropbox/PhD/classes/STAT695/Titanic")

require(knitr)
require(dplyr)
require(tidyr)
require(matrixStats)
require(ggplot2)
require(class)
require(e1071)
require(caret)


train <- read.csv("data/train.csv") %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Name = as.character(Name),
         Sex = factor(Sex))

na_count_train <-sapply(train, function(y) sum(length(which(is.na(y)))))
(na_count_train <- data.frame(na_count_train))
NAindex_train_age <- which(is.na(train$Age))
str(train)


test <- read.csv("data/test.csv") %>%
  mutate(Pclass = factor(Pclass),
         Name = as.character(Name),
         Sex = factor(Sex))

na_count_test <-sapply(test, function(y) sum(length(which(is.na(y)))))
(na_count_test <- data.frame(na_count_test))
NAindex_test <- which(is.na(test))
NAindex_test_fare <- which(is.na(test$Fare))
NAindex_test_age <- which(is.na(test$Age))
str(test)

train_x <- select(train, Fare)
train_y <- train$Survived
test_x <- select(test,Fare)
test_x$Fare[NAindex_test_fare] <- mean(test$Fare, na.rm = TRUE)
set.seed(1201)
train_pred_fare <- knn(train_x, train_x, train_y, k = 1)
head(train_pred_fare)

knn_eval_Fare <- confusionMatrix(data = train_pred_fare,
                           reference = train$Survived,
                           positive = "1")
knn_eval_Fare$byClass

test_pred_fare <- knn(train_x, test_x, train_y, k = 5)

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = test_pred_fare)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "predictions/fare_knn5.csv", row.names = FALSE)




############################################################

# Set up choices
predictors <- c("Sex", "Pclass")

#predictors <- c("Sex") #Doesn't work for some reason.
k <- 5

# Limit to complete cases with the outcome and selected predictors
train_knn <- train[ , c("Survived", predictors)] 
train_knn <- train_knn[complete.cases(train_knn), ]
train_x <- as.data.frame(train_knn[ , predictors])

# Convert to the model matrix to handle categorical variables
colnames(train_x) <- predictors
(model_formula <- as.formula(paste("~", 
                                   paste(predictors, collapse = " + "))))
train_x <- model.matrix(model_formula, data = train_x)[ , -1]
head(train_x, 3)

# Scale predictors
train_x <- apply(train_x, 2, scale)
#train_x <- as.integer(train_x)
head(train_x, 3)

# Create vector of outcomes
train_y <- train_knn$Survived

# Fit model
set.seed(1201)
train_pred <- knn(train_x, train_x, train_y, k = 5)


#############################################################


train$Sex_n <- as.numeric(train$Sex)
train_knn <- as.matrix(train[ , "Sex_n"])

set.seed(1201)
train_pred_sex <- knn(train_knn, train_knn, train$Survived, k = 1)

head(train_pred_sex)

knn_eval_Sex <- confusionMatrix(data = train_pred_sex,
                                reference = train$Survived,
                                positive = "1")
knn_eval_Sex$byClass

test$Sex_n <- as.numeric(test$Sex)
test_knn <- as.matrix(test[ , "Sex_n"])
test_pred_sex <- knn(train_knn, test_knn, train$Survived, k = 1)

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = test_pred_sex)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "predictions/sex_knn1.csv", row.names = FALSE)
