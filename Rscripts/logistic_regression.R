rm(list=ls())

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

glm.fit=glm(Survived~Sex+Fare ,
            data=train ,family =binomial )
summary (glm.fit )

glm.probs = predict(glm.fit ,type ="response")
head(glm.probs)
(length(glm.probs))

glm.pred=rep (0 , length(glm.probs))
glm.pred[glm.probs >.5]=1

mean(glm.pred== train$Survived)

eval_SexFare <- confusionMatrix(data = glm.pred,
                                 reference = train$Survived,
                                 positive = "1")
eval_SexFare$byClass

glm.probs_test = predict(glm.fit, test ,type ="response")
glm.pred_test=rep (0 , length(glm.probs_test))
glm.pred_test[glm.probs_test >.5]=1

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = glm.pred_test)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "predictions/log_SexFare.csv", row.names = FALSE)


