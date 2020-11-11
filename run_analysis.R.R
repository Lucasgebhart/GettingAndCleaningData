#Getting and Cleaning Data Project
#preparation
setwd("C:/users/lucas.gebhart/Desktop/Coursera/projectdata/")
library(dplyr)
#Merge the test and train data into one data set
#begin by reading in all of the data, seeing what it is, and giving names to columns

#Overall
features <-read.table("features.txt")
head(features)
colnames(features) <- c("featurenumber", "featurename")

activity <- read.table("activity_labels.txt")
head(activity)
colnames(activity) <- c("activitynumber", "activity")
View(activity)

#Test Data
subject_test <- read.table('test/subject_test.txt')
head(subject_test)
colnames(subject_test) <-"subject"

x_test <- read.table('test/X_test.txt')
head(x_test)
colnames(x_test) <- features$featurename

y_test <-read.table('test/y_test.txt')
head(y_test)
colnames(y_test) <-"activitynumber"
View(y_test)

#Train Data
subject_train <- read.table('train/subject_train.txt')
head(subject_train)
colnames(subject_train) <-"subject"

x_train <- read.table('train/X_train.txt')
head(x_train)
colnames(x_train) <- features$featurename

y_train <-read.table('train/y_train.txt')
head(y_train)
colnames(y_train) <-"activitynumber"


#Task 1: Merges the training and the test sets to create one data set.
x_complete <- rbind(x_test, x_train)
y_complete <- rbind(y_test, y_train)
subject_complete <-rbind(subject_test, subject_train)
merged_data <- cbind(subject_complete, x_complete, y_complete)
head(merged_data)
View(merged_data)

#Task 2: Extracts only the measurements on the mean and standard deviation for each measurement. Include subject ID and activity number
str(merged_data)
extracted_data<- merged_data[grep("std|mean|activitynumber|subject", names(merged_data), ignore.case = TRUE)]
str(extracted_data)

#Task 3: Uses descriptive activity names to name the activities in the data set
extracted_data$activitynumber <-as.character(extracted_data$activitynumber)
for (i in 1:6){
    extracted_data$activitynumber[extracted_data$activitynumber == i] <- as.character(activity[i,2])
}   


#Task 4: Appropriately labels the data set with descriptive variable names.
names(extracted_data)

#Move the activity type so that the data now reads subject, activity, various measures
extracted_data <- extracted_data[,c(1, 88, 2:87)]
names(extracted_data)

#replace bodybody with Body
names(extracted_data)<- gsub("bodybody", "Body", names(extracted_data), ignore.case = TRUE)

#Replace f with Frequency
names(extracted_data)<- gsub("^f", "Frequency", names(extracted_data), ignore.case = TRUE)

#Replace t with Time
names(extracted_data)<- gsub("^t", "Time", names(extracted_data), ignore.case = TRUE)

#Replace tBody with TimeBody
names(extracted_data)<- gsub("tBody", "TimeBody", names(extracted_data), ignore.case = TRUE)

#Replace acc with Accelerometer
names(extracted_data)<- gsub("acc", "Accelerometer", names(extracted_data), ignore.case = TRUE)

#Replace mag with Magnitude
names(extracted_data)<- gsub("mag", "Magnitude", names(extracted_data), ignore.case = TRUE)

#Replace activitynumber with Activity, since number is no longer what this is....
names(extracted_data)<- gsub("activitynumber", "Activity", names(extracted_data), ignore.case = TRUE)

#Bring all of the variables to the same capitalization schema.Try and remove unnecessary () (failed at that?)
names(extracted_data)<- gsub("subject", "Subject", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<- gsub("gravity", "Gravity", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<- gsub("angle", "Angle", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<- gsub("mean()", "Mean", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<- gsub("std()", "STD", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<- gsub("freq()", "Freq", names(extracted_data), ignore.case = TRUE)


#Task 5:From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable for each activity and each subject.
tidyExtraction <- extracted_data %>% group_by(Subject, Activity) %>% summarise_all(list(mean))
write.table(tidyExtraction, file="tidyExtraction.txt", row.names = FALSE)


