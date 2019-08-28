#1. Merges the training and the test sets to create one data set.

# load libraries
library(dplyr) 


### read files

#train data 
x_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/Y_train.txt") 
sub_train <- read.table("./train/subject_train.txt")

#test data 
x_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/Y_test.txt") 
sub_test <- read.table("./test/subject_test.txt")

#features description 
features <- read.table("./features.txt") 

#activity labels 
activity_labels <- read.table("./activity_labels.txt") 

#merge training and test sets
x_total <- rbind(x_train, x_test)
y_total <- rbind(y_train, y_test) 
sub_total <- rbind(sub_train, sub_test) 


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

sel_features <- features[grep(".*mean\\(\\)|std\\(\\)", features[,2], ignore.case = FALSE),]
x_total      <- x_total[,sel_features[,1]]


# 3. Uses descriptive activity names to name the activities in the data set

colnames(y_total) <- "activity"
y_total$activitylabel <- factor(y_total$activity, labels = as.character(activity_labels[,2]))
activitylabel <- y_total[,-1]

# 4. Appropriately labels the data set with descriptive variable names.
colnames(x_total) <- features[sel_features[,1],2]


# 5. From the data set in step 4, creates a second, independent tidy data set with the average
# of each variable for each activity and each subject.
colnames(sub_total) <- "subject"
total <- cbind(x_total, activitylabel, sub_total)
total_mean <- total %>% group_by(activitylabel, subject) %>% summarize_each(funs(mean))
write.table(total_mean, file = "./tidydata.txt", row.names = FALSE, col.names = TRUE)
