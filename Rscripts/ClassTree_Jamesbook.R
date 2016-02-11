rm(list=ls())

setwd("C:/Docs/Dropbox/PhD/classes/STAT695/Titanic")

library("tree")

# Import the training set: train
train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
train <- read.csv(train_url)

# Import the testing set: test
test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
test <- read.csv(test_url)

tree.Titanic =tree(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,  train)
summary(tree.Titanic)
#create a regression tree for some reason...

#tree.pred=predict (tree.Titanic, test, type ="class")#Doesn't work as a regression tree is generated not a classification tree
tree.pred=predict (tree.Titanic, test)

pred_test=rep (0 , length(tree.pred))
pred_test[tree.pred >.5]=1

pred_train <- predict(tree.Titanic, train)
pred_train=rep (0 , length(pred_train))
pred_train[pred_train >.5]=1
mean(pred_train == "1") ## For a T / F, gives the proportion T's
sum(pred_train == train$Survived) / length(pred_train)

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = pred_test)

# Check that your data frame has 418 entries
nrow(my_solution)

# Write your solution to a csv file with the name my_solution.csv
write.csv(my_solution, file = "predictions/ClassTree_Jamesbook.csv", row.names = FALSE)

plot(tree.Titanic)
text(tree.Titanic ,pretty =0)
tree.Titanic
