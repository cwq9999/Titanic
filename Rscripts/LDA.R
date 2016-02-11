rm(list=ls())

require(knitr)
require(dplyr)
require(tidyr)
require(matrixStats)
require(ggplot2)
require(class)
require(e1071)
require(MASS)
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

#lda.fit=lda(Survived~Sex+Fare ,data=train, subset = xyz)
lda.fit=lda(Survived~Sex+Fare ,data=train)

summary (lda.fit)

lda.pred = predict(lda.fit)
names(lda.pred)

lda.class = lda.pred$class
table(lda.class, train$Survived)
(mean(lda.class==train$Survived))

eval_SexFare <- confusionMatrix(data = lda.class,
                                reference = train$Survived,
                                positive = "1")
eval_SexFare$byClass

test_x <- test
test_x$Fare[NAindex_test_fare] <- mean(test$Fare, na.rm = TRUE)
lda.testpred <- predict(lda.fit, test_x)
names(lda.testpred)

lda.testpred = lda.testpred$class
length(lda.testpred)

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = lda.testpred)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "predictions/lda_SexFare.csv", row.names = FALSE)



