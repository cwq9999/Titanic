# Import the training set: train
train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
train <- read.csv(train_url)

# Import the testing set: test
test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
test <- read.csv(test_url)

# Print train and test to the console
train
test

str(train)
str(test)

# Your train and test set are still loaded
str(train)
str(test)

# Passengers that survived vs passengers that passed away
table(train$Survived) 

# As proportions
prop.table(table(train$Survived))

# Males & females that survived vs males & females that passed away
table(train$Sex, train$Survived)

# As row-wise proportions
prop.table(table(train$Sex, train$Survived), margin =1)

# Create the column child, and indicate whether child or no child
train$Child <- NA
train$Child[train$Age < 18] <- 1
train$Child[train$Age >= 18] <- 0

# Two-way comparison
prop.table(table(train$Child, train$Survived), 1)

# Create a copy of test: test_one
test_one <- test

# Initialize a Survived column to 0
test_one$Survived <- 0

# Set Survived to 1 if Sex equals "female"
test_one$Survived[test_one$Sex == "female"] <- 1