##############################################################################
#
# FILE
#   run_analysis.R
#
# OVERVIEW
#   Using data collected from the accelerometers from the Samsung Galaxy S 
#   smartphone, work with the data and make a clean data set, outputting the
#   resulting tidy data to a file named "tidy_data.txt".
#   See README.md for details.
#

library(dplyr)


##############################################################################
# STEP 0A - Get data
##############################################################################

# download zip file containing data if it hasn't already been downloaded
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}
##############################################################################
# STEP 0B - Read data
##############################################################################

# read training data
trainSub<-read.table(file.path(dataPath,"train","subject_train.txt"),header = FALSE)
trainVal<-read.table(file.path(dataPath,"train","X_train.txt"),header = FALSE)
trainAct<-read.table(file.path(dataPath,"train","y_train.txt"),header=FALSE)

# read test data
testSub<-read.table(file.path(dataPath,"test","subject_test.txt"),header=FALSE)
testVal<-read.table(file.path(dataPath,"test","X_test.txt"),header=FALSE)
testAct<-read.table(file.path(dataPath,"test","y_test.txt"),header=FALSE)

# read features,without converting
features<-read.table(file.path(dataPath,"features.txt"), as.is=TRUE,header=FALSE)

#read activities Label
activities<-read.table(file.path(dataPath,"activity_labels.txt"),header=FALSE)
colnames(activities)<-c("activityID","activityLabel")

##############################################################################
# Step 1 - Merge the training and the test sets to create one data set
##############################################################################

# concatenate individual data tables to make single data table

Dataset<-rbind(cbind(trainSub,trainVal,trainAct),
               cbind(testSub,testVal,testAct))



# remove individual data tables to save memory
rm(trainSub, trainVal, trainAct, 
   testSub, testVal, testAct)


# assign column names

colnames(Dataset)<-c("subject",features[,2],"activity")

##############################################################################
# Step 2 - Extract only the measurements on the mean and standard deviation
#          for each measurement
##############################################################################



# determine columns of data set to keep based on column name...

KeepCol<-grepl("subject|activity|mean|std",colnames(Dataset))


# ... and keep data in these columns only

Dataset<-Dataset[,KeepCol]


# replace activity values with named factor levels to make it categorical variable

Dataset$activity<-factor(Dataset$activity,
                         levels = activities[,1],labels = activities[,2])


##############################################################################
# Step 4 - Appropriately label the data set with descriptive variable names
##############################################################################


# get column names
DatasetCols<-colnames(Dataset)


# remove special characters

DatasetCols<-gsub("[\\(\\)-]","",DatasetCols)


# expand abbreviations and clean up names

DatasetCols<-gsub("^f","frequencyDomain",DatasetCols)
DatasetCols<-gsub("^t","timeDomain",DatasetCols)
DatasetCols<-gsub("^Acc","Accelerometer",DatasetCols)
DatasetCols<-gsub("Gyro","Gyroscope",DatasetCols)
DatasetCols<-gsub("Mag","Magnitude",DatasetCols)
DatasetCols<-gsub("Freq","Frequency",DatasetCols)
DatasetCols<-gsub("mean","Mean",DatasetCols)
DatasetCols<-gsub("std","StandardDeviation",DatasetCols)

#correct typo

DatasetCols<-gsub("BodyBody","Body",DatasetCols)


#use labels as column names

colnames(Dataset)<-DatasetCols

##############################################################################
# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
##############################################################################

#group by subject and activity and summarise using mean

DatasetMean<-Dataset %>%
  group_by(subject,activity) %>%
  summarise_all(funs(mean))

#output

write.table(DatasetMean,"tidy_data.txt",row.names = FALSE, quote = FALSE)



library(knitr)
knit2html("codebook.Rmd");








































