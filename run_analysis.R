#The program will do
# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation 
#   for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.From the data set in step 4, creates a second, independent tidy data set 
#   with the average of each variable for each activity and each subject.

#load library
library(dplyr)

#Load data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
download.file(url, destfile="./data/rawdata.zip",mode = "wb")
unzip("./data/rawdata.zip", exdir="./data")

#load labels and feature
labels <- read.delim("./data/UCI HAR Dataset/activity_labels.txt", header = FALSE, sep=" ",col.names=c("labels","activity"))
feature <- read.delim("./data/UCI HAR Dataset/features.txt", header = FALSE, sep=" ",col.names=c("index","feature"))

#load training data
training_set <- read.delim("./data/UCI HAR Dataset/train/X_train.txt", header = FALSE, col.names=c("measurements"))
training_label <- read.delim("./data/UCI HAR Dataset/train/Y_train.txt", header = FALSE, col.names=c("label"))
training_subject <- read.delim("./data/UCI HAR Dataset/train/subject_train.txt", header = FALSE, col.names=c("subject"))
train = cbind(training_set,training_label,training_subject)
train$usage="training"
#load  testing data
testing_set <- read.delim("./data/UCI HAR Dataset/test/X_test.txt", header = FALSE, col.names=c("measurements"))
testing_label <- read.delim("./data/UCI HAR Dataset/test/Y_test.txt", header = FALSE, col.names=c("label"))
testing_subject <- read.delim("./data/UCI HAR Dataset/test/subject_test.txt", header = FALSE, col.names=c("subject"))
test = cbind(testing_set,testing_label,testing_subject)
test$usage="testing"

#merge test and train data
tidyData <- rbind(train,test)

#change labels to be descriptive
tidyData$label <- factor(tidyData$label, levels = labels$labels, labels = labels$activity)

#Feature to be extracted
extract <- filter(feature, grepl("mean()|std()" ,feature ))

#adding mean of measurement
temp <- sapply(tidyData$measurements, function(x){
  na.omit(as.numeric(unlist(strsplit(x," "))))
})
for(i in extract$index){
  tidyData <- cbind(temp[i,],tidyData)
}
names(tidyData)[1:79] <-extract$feature

#adding mean for each label
temp <- extract$feature 
tempp <- paste(temp, "-mean-label")
for(i in 1:79){
  tidyData[[tempp[i]]] <- ave(tidyData[[temp[i]]], tidyData$label, FUN = mean)
}

#adding mean for each subject.
tempp <- paste(temp, "-mean-subject")
for(i in 1:79){
  tidyData[[tempp[i]]] <- ave(tidyData[[temp[i]]], tidyData$subject, FUN = mean)
}

#writing dataset to text file
write.table(tidyData[,81:241], file="tidyData.txt" ,row.name=FALSE) 
