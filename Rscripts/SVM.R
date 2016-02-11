rm(list=ls())

setwd("C:/Docs/Dropbox/PhD/classes/STAT695/Titanic")

require(knitr)
require(dplyr)
require(tidyr)
require(matrixStats)
require(ggplot2)
require(class)
require(e1071)
#require(caret)


train <- read.csv("data/train.csv") %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Name = as.character(Name),
         Sex = factor(Sex),
         Embarked = factor(ifelse(Embarked == "", NA, as.character(Embarked))))

# train <- read.csv("data/train.csv") %>%
#   mutate(Survived = factor(Survived),
#          Pclass = factor(Pclass),
#          Name = as.character(Name),
#          Sex = factor(Sex))

na_count_train <-sapply(train, function(y) sum(length(which(is.na(y)))))
(na_count_train <- data.frame(na_count_train))
NAindex_train_age <- which(is.na(train$Age))
str(train)


test <- read.csv("data/test.csv") %>%
  mutate(Pclass = factor(Pclass),
         Name = as.character(Name),
         Sex = factor(Sex),
         Embarked = factor(ifelse(Embarked == "", NA, as.character(Embarked))))

na_count_test <-sapply(test, function(y) sum(length(which(is.na(y)))))
(na_count_test <- data.frame(na_count_test))
NAindex_test <- which(is.na(test))
NAindex_test_fare <- which(is.na(test$Fare))
NAindex_test_age <- which(is.na(test$Age))
str(test)



set.seed(1)

# svmfit =svm(Survived ~ Pclass + Sex + SibSp + Parch, data=train, kernel ="poynomial", degree =2,  cost =1)

svmfit =svm(Survived ~ Pclass + Sex + SibSp + Parch, data=train, kernel ="radial", gamma =0.5,  cost =1)

summary(svmfit)

svm.predtrain=predict(svmfit, train) #Can't predict still with some of the avilable parameters....
mean(svm.predtrain == "1") ## For a T / F, gives the proportion T's
sum(svm.predtrain == train$Survived) / length(svm.predtrain)

svm.predtest=predict(svmfit, test) #Can't predict still with some of the avilable parameters....
#Error in names(ret2) <- rowns : 
#'names' attribute [418] must be the same length as the vector [331]

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = svm.predtest)

# Write your solution away to a csv file with the name my_solution.csv
#write.csv(my_solution, file = "predictions/svm_PclasSexsibPar.csv", row.names = FALSE)
write.csv(my_solution, file = "predictions/svm_PclasSexsibParRadial.csv", row.names = FALSE)

#tune.out=tune(svm, Survived~.-Name, data=train, kernel ="polynomial",
#              ranges =list(cost=c(0.1 ,1 ,10 ,100), degree=c(3,4)))
 
tune.out=tune(svm, Survived ~ Pclass + Sex + SibSp + Parch, data=train, kernel ="polynomial",
             ranges =list(cost=c(0.1, 1, 10, 100), degree=c(3, 4) ))

# Still doesn't work even with Brooke's suggestion... get: 
#Error in names(ret2) <- rowns : 
#   'names' attribute [90] must be the same length as the vector [77]

summary (tune.out)

pred.besttrain=predict(tune.out$best.model, train)

mean(pred.besttrain == "1") ## For a T / F, gives the proportion T's
sum(pred.besttrain == train$Survived) / length(pred.besttrain)

pred.besttest=predict(tune.out$best.model, test)

mytuned_solution <- data.frame(PassengerId = test$PassengerId, Survived = pred.besttest)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(mytuned_solution, file = "predictions/svmtunedPclasSexsibPar.csv", row.names = FALSE)

