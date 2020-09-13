#Getting and Cleaning Data Assignment 


#Load packages and get Data
library(reshape2)
library(data.table)
library(dplyr)
path <- getwd()

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(url, file.path(path, "dataFiles.zip"))

unzip(zipfile = "dataFiles.zip")

#read in the datasets and assignment of column names
#read feature vector
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n", "functions"))

##read activity labels
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("activityid", "activitytype"))

#read training sets
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names =features$functions)

y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

#read testing sets 
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)

y_test<- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")

subject_test<- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")


#Merging all of the data into one set

X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
subject <- rbind(subject_test, subject_train)
mergedData <- cbind(subject,X,Y)


# Extracting only measurements of mean and standard deviation for each measurement

specificVariables <- features[grep("mean\\(\\)|std\\(\\)", features[,2]),]

X <- X[, specificVariables[,1]]

#Use descriptive activity names to name the activities in the data-set

colnames(Y)<- "activity"
Y$activity_label <- factor(Y$activity, labels = as.character(activity_labels[,2]))
activitylabel<- Y[,-1]

#Label the dataset with descriptiv variable names

colnames(X)<- features[specificVariables[,1],2]

#From dataset X create a second tidy data set with the average of each variable for each activity & subject

colnames(subject)<- "subject"
total <- cbind(X,activitylabel,subject)
totalMean <- total%>% group_by(activitylabel, subject)%>% summarise_each(funs(mean))
write.table(totalMean, file = "./UCI HAR Dataset/tidydata.txt", row.names = F, col.names = T)
