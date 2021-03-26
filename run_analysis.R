#The program will do
# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation 
#   for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.From the data set in step 4, creates a second, independent tidy data set 
#   with the average of each variable for each activity and each subject.

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

#adding mean of measurement
tidyData$mean = sapply(tidyData$measurements, function(x){
  mean(as.numeric(unlist(strsplit(x," "))), na.rm = TRUE)
})

#adding standart deviation of measurement
tidyData$standart_deviation = sapply(tidyData$measurements, function(x){
  sd(as.numeric(unlist(strsplit(x," "))), na.rm = TRUE)
})

#adding mean for each label
tidyData$mean_label <- ave(tidyData$mean,tidyData$label, FUN = mean)

#adding mean for each subject
tidyData$mean_subject <- ave(tidyData$mean,tidyData$subject, FUN = mean)

#writing dataset to text file
write.table(tidyData[,2:8], file="tidyData.txt" ,row.name=FALSE) 
