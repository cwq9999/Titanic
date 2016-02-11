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
nb_mod <- naiveBayes(Survived ~ Pclass, data = train)

nb_mod$apriori ## Class distribution for `Survived`
nb_mod$tables ## Conditional probabilities given of `Pclass` given `Survived`

apply(nb_mod$tables$Pclass, 1, sum)

pred_train <- predict(nb_mod, train)
mean(pred_train == "1") ## For a T / F, gives the proportion T's

table(pred_train, train$Survived, train$Pclass)
sum(pred_train == train$Survived) / length(pred_train)

pred_test <- predict(nb_mod, test)

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
Brookes_solution <- data.frame(PassengerId = test$PassengerId, Survived = pred_test)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(Brookes_solution, file = "predictions/Brookes_NB_Pclass.csv", row.names = FALSE)

###################################
###################################
###################################

mosaicplot(~ Pclass + Sex, data = train, color = TRUE, main = "")

nb_mod <- naiveBayes(Survived ~ Pclass + Sex + Age + Fare, data = train)

pred_train <- predict(nb_mod, train)
mean(pred_train == "1") ## For a T / F, gives the proportion T's
sum(pred_train == train$Survived) / length(pred_train)

pred_test <- predict(nb_mod, test)


# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = pred_test)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "predictions/NB_Pc_Sex_Age_Fare.csv", row.names = FALSE)


