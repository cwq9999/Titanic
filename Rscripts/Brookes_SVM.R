rm(list=ls())

setwd("C:/Docs/Dropbox/PhD/classes/STAT695/Titanic")

library(dplyr) # Data wrangling
library(tidyr) # Data wrangling
library(caret) # Machine learning
library(ggplot2) # Plotting
library(VIM) # k-NN imputation
library(stringr) # For string manipulation
library(mice)

train <- read.csv("data/train.csv") %>%
  select(Survived, Pclass, Sex, Age) %>%
  mutate(Survived = factor(Survived),
         Pclass = ordered(Pclass),
         Sex = factor(Sex))

test <- read.csv("data/test.csv") %>%
  select(Pclass, Sex, Age) %>%
  mutate(Pclass = ordered(Pclass),
         Sex = factor(Sex))

test_ids <- read.csv("data/test.csv") %>% 
  select(PassengerId)

set.seed(1201)
svm_fit <- train(Survived ~ .,
                 data = train,
                 method = "svmRadial",
                 preProc = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))
svm_fit

plot(svm_fit)

preds <- predict(svm_fit, newdata = test)

length(preds)
dim(test)
sum(complete.cases(test))

test2 <- kNN(test)
head(test2)

preds2 <- predict(svm_fit, newdata = test2)
length(preds2)

################################################
################################################
################################################

train <- read.csv("data/train.csv") %>%
  select(Survived, Pclass, Name, Sex, Age, SibSp, Parch, Fare, Embarked) %>%
  mutate(Survived = factor(Survived),
         Pclass = ordered(Pclass),
         Name = gsub("[\\,\\.\\ ]", "", str_extract(Name, ",\\ .+?\\.")),
         Name = factor(ifelse(Name %in% c("Mr", "Mrs", "Miss", "Master"),
                              Name, "Other")),
         Sex = factor(Sex),
         Embarked = factor(ifelse(Embarked == "", NA, as.character(Embarked))))
test <- read.csv("data/test.csv") %>%
  select(Pclass, Name, Sex, Age, SibSp, Parch, Fare, Embarked) %>%
  mutate(Pclass = ordered(Pclass),
         Name = gsub("[\\,\\.\\ ]", "", str_extract(Name, ",\\ .+?\\.")),
         Name = factor(ifelse(Name %in% c("Mr", "Mrs", "Miss", "Master"),
                              Name, "Other")),
         Sex = factor(Sex),
         Embarked = factor(ifelse(Embarked == "", NA, as.character(Embarked))))
test_ids <- read.csv("data/test.csv") %>% 
  select(PassengerId)

set.seed(1201)
svm_fit <- train(Survived ~ .,
                 data = train,
                 method = "svmRadial",
                 preProc = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))
svm_fit

test2 <- kNN(test)
head(test2)

preds2 <- predict(svm_fit, newdata = test2)
length(preds2)



