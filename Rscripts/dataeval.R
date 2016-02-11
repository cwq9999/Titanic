rm(list=ls())

setwd("C:/Docs/Dropbox/PhD/classes/STAT695/Titanic")

require(knitr)
require(dplyr)
require(tidyr)
require(VIM)
require(mice)

train <- read.csv("data/train.csv") %>%
  mutate(Survived = factor(Survived),
  Pclass = factor(Pclass),
  Name = as.character(Name),
  Sex = factor(Sex))

test <- read.csv("data/test.csv") %>%
  mutate(Pclass = factor(Pclass),
  Name = as.character(Name),
  Sex = factor(Sex))

aggr_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
(train_md <- md.pattern(train))

aggr_plot <- aggr(test, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
(test_md <- md.pattern(test))