rm(list=ls())

setwd("C:/Docs/Dropbox/PhD/classes/STAT695/Titanic")

library(tree)

# Load in the packages to create a fancified version of your tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)
require(caret)

# Import the training set: train
train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
train <- read.csv(train_url)

# Import the testing set: test
test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
test <- read.csv(test_url)

# Build the decision tree
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method ="class")

pred_train <- predict(my_tree_two, train, type = "class")
mean(pred_train == "1") ## For a T / F, gives the proportion T's
sum(pred_train == train$Survived) / length(pred_train)

# Visualize the decision tree using plot() and text()
plot(my_tree_two)
text(my_tree_two)

# Time to plot your fancy tree
#fancyRpartPlot(my_tree_two)

# Make your prediction using the test set
my_prediction <- predict(my_tree_two, test, type = "class")

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Check that your data frame has 418 entries
nrow(my_solution)

# Write your solution to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

# Create a new decision tree my_tree_three
my_tree_three <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class", control = rpart.control(minsplit = 50, cp = 0))

# Visualize your new decision tree
#fancyRpartPlot(my_tree_three)

# create a new train set with the new variable
train_two <- train
train_two$family_size <- train$SibSp + train$Parch + 1

# Create a new decision tree my_tree_three
my_tree_four <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size, data = train_two, method="class")

# Visualize your new decision tree
#fancyRpartPlot(my_tree_four)

# create a new train set with the new variable
test_two <- test
test_two$family_size <- test$SibSp + test$Parch + 1

pred_train <- predict(my_tree_four, train_two, type = "class")
mean(pred_train == "1") ## For a T / F, gives the proportion T's
sum(pred_train == train$Survived) / length(pred_train)

# # train_new and test_new are available in the workspace
# str(train_new)
# str(test_new)
# 
# # Create a new model `my_tree_five`
# my_tree_five <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data = train_new, method="class")
# 
# # Visualize your new decision tree
# fancyRpartPlot(my_tree_five)
# 
# # Make your prediction using `my_tree_five` and `test_new`
# my_prediction <- predict(my_tree_five, test_new, type = "class")
# 
# # Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
# my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)
# 
# # Write your solution away to a csv file with the name my_solution.csv
# write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)