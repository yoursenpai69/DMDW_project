#installing and importing required packages
install.packages("tidyverse")
install.packages("class")
install.packages("gmodels")
install.packages("caret")
install.packages('e1071', dependencies=TRUE)
library(tidyverse)
library(class)
library(caret)
library(gmodels)
#importing the dataset
netflix <- read.csv("D:/PROJECTS/DMDW project/netflix_titles.csv")
# creating a subset of our datset with the required predictor variables
netflix.subset <- netflix[c('show_id', 'release_year', 'rating', 'country', 'type')]
netflix.subset[netflix.subset==""] <- NA
#viewing a part of the dataset to check if it is correct
head(netflix.subset)
#used to see how the dataset looks
str(netflix.subset)
x <- unique(netflix.subset[c("release_year")])

#checking for incomplete dataset
sum(is.na(netflix.subset$country))

netflix.subset['show_id','release_year'] <- lapply(netflix.subset['show_id','release_year'],as.numeric)
netflix.subset['type'] <- lapply(netflix.subset['type'],as.numeric)

#correcting the incomplete rows
netflix.subset <- na.omit(netflix.subset)
sum(is.na(netflix.subset$country))

#one-hot encoding the dataframe
dummy <- dummyVars(" ~ .", data=netflix.subset[, c("show_id", "rating", "country")])
newdata <- data.frame(predict(dummy, newdata = netflix.subset[, c("show_id", "rating", "country")]))

#droping and merging into final dataframe
drop <- c("country","rating")
netflix.subset = netflix.subset[,!(names(netflix.subset) %in% drop)]
head(netflix.subset)
netflix.subset  = merge(netflix.subset, newdata, by.x = "show_id")
head(netflix.subset)

#creating training and testing set
set.seed(123)
dat.d <- sample(1:nrow(netflix.subset),size=nrow(netflix.subset)*0.7,replace = FALSE) #random selection of 70% data.

train.netflix <- netflix.subset[dat.d,] # 70% training data
test.netflix <- netflix.subset[-dat.d,] # remaining 30% test data

#Creating separate dataframe for 'type' feature which is our target.
train.netflix_labels <- netflix.subset[dat.d,3]
test.netflix_labels <-netflix.subset[-dat.d,3]

#Find the number of observation
NROW(train.netflix_labels) 
knn.40 <- knn(train=train.netflix, test=test.netflix, cl=train.netflix_labels, k=40)


#Calculate the proportion of correct classification for k = 71, 72
ACC.40 <- 100 * sum(test.netflix_labels == knn.71)/NROW(test.netflix_labels)


#creating a confusion matrix
confusionMatrix(table(knn.71 ,test.netflix_labels))

#evaluation with cross table
CrossTable(test.netflix_labels, knn.40)


