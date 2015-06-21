###############################################################################
# Project 1 data:
# 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 
# full dataset description:
#
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
#
# Project 1 Instructions: 
#
# You should create one R script called run_analysis.R that does the following:
#
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each 
# measurement. 
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names. 
# 5) From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
###############################################################################


DownloadAndUnzip <- function() {
    
    # Download  dataset
    if(!file.exists("data")){
        dir.create("data")
    }
    
    if (!file.exists("./data/project1_data.zip")) {
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" 
        download.file(fileUrl, destfile = "./data/project1_data.zip", method="curl")
    }
    
    #unzip archive
    if (!file.exists("data/UCI\ HAR\ Dataset")){
        unzip("./data/project1_data.zip",exdir="./data")
    }
    
}



########################################################################
# Step1: Merge the training and the test sets to create one data set.
########################################################################

# source foulders: train/valid
# source files: 
# X_train.txt/X_test.txt 
# y_train.txt/y_test.txt
# subject_train.txt/subject_test.txt

MergeTrainAndTestData <- function(save2file){
    
    # Load activity labels data : 7352/2947  rows for 1 column
    activity_data_train <- read.table("./data/UCI\ HAR\ Dataset/train/y_train.txt", header = FALSE, col.names = "Activity")
    activity_data_test <- read.table("./data/UCI\ HAR\ Dataset/test/y_test.txt", header = FALSE, col.names = "Activity")
    # merge rows
    activity_data_all <- rbind(activity_data_train, activity_data_test)
    
    # Load subject data : 7352/2947 rows for 1 column
    subject_data_train <- read.table("./data/UCI\ HAR\ Dataset/train/subject_train.txt", header = FALSE, col.names = "Subject")
    subject_data_test <- read.table("./data/UCI\ HAR\ Dataset/test/subject_test.txt", header = FALSE, col.names = "Subject")
    # merge rows
    subject_data_all <-rbind(subject_data_train,subject_data_test)
    #subject_data_all <- as.factor(subject_data_all)
    
    # Load features data : 7352/2947 rows 561 columns
    feat_data_train <- read.table("./data/UCI\ HAR\ Dataset/train/X_train.txt", header = FALSE)
    feat_data_test <- read.table("./data/UCI\ HAR\ Dataset/test/X_test.txt", header = FALSE)
    # merge rows
    feat_data_all <- rbind(feat_data_train, feat_data_test)
    
    
    
    # Finally, merge these 3 frames by columns : | Activity | Subject | Features
    merged_data_all <- cbind(activity_data_all, cbind(subject_data_all, feat_data_all))
    
    
    # if required, save frame in file
    if (save2file) {
     write.csv(merged_data_all, file = "./data/merged_data.csv", row.names = FALSE)
    }
    
    return(merged_data_all)
    
}

#####
# Find the names and the indices of the mean and standard deviation for each 
# measurement
# Output: the list of names of the required variables, and their corresponding 
# column indices, obtained from the file "features.txt"

GetFeatures <- function() {
    
    # The names of the features are stored in the file "features.txt"
    feature_names <- read.table("data/UCI\ HAR\ Dataset/features.txt", header = FALSE, as.is = TRUE)
    feature_names <- feature_names$V2
    
    # Find rows indices whose feature names contains either the words "mean()" or "std()"
    feature_indices <- c(grep("mean()",feature_names,fixed = TRUE),grep("std()",feature_names,fixed = TRUE))    
    feature_names <- feature_names[feature_indices]
    
    # this indices will be used later on to change the corresponding column names in the merged data frame
    # I have to shift them by "2" because the merged data frame also contains Activity and Subject columns
    return(list("names" = feature_names , "indices" = feature_indices+2))
}


###############################################################################
# Step 2 : Extract only the measurements on the mean and standard deviation 
# for each measurement. 
###############################################################################

FilterData <-function (data,features) {
    
    #library(dplyr)
    feature_indices = features[["indices"]]
    
    # Select only those columns from the mergedDataSet (which has been created  as follows:
    # rows = time 
    # columns = activity | subject | measurements 
    mod_data <- data[,c(1,2,feature_indices)] 
    
    #col_names <- c(feature_names, "Activity","Subject")
    return(mod_data)
}

####################################################################
# Step 3: replace activity values with descriptive activity names
####################################################################
DescribeActivity <- function(data){
    
    #library(dplyr)
    # The correspondence between an activity label and its name is stored in 
    # activity_labels.txt
    activity_names <- read.table("data/UCI\ HAR\ Dataset/activity_labels.txt",header = FALSE)
    
    # transform this activity names into a factor
    activity_names_fac <- as.factor(activity_names$V2)
    
    # this is the original activity column data. Make it a factor
    activity_label_fac <- as.factor(data$Activity)
    
    # remap the levels of one factor into the other
    new_activity_data <- plyr::mapvalues(activity_label_fac, levels(activity_label_fac), levels(activity_names_fac))
    
    # put the new relabeled factor column in the original dataset
    # (This could be made with a lot less code, but I am making each passage explicit for readability)
    data[,"Activity"] <- new_activity_data
    return(data)
}

##############################################################################
# Step 4: Appropriately label the data set with descriptive variable names. 
##############################################################################
AddColumnNames <- function(data, features) {
        
    feature_names = features[["names"]]
    #feature_indices = features[["indices"]]
    
    for (i in 1:length(feature_names)){
        
        # for the first columns, use original feature names, to obtain a more
        # decriptive and valid column name in R
        new_name  <- ""
        old_name <- feature_names[i]
        
        if(grepl("mean()", old_name)){
            new_name <- paste(new_name, "Mean", sep = "")
        }
        
        if(grepl("std()", old_name)) {
            new_name <- paste(new_name, "Std", sep = "")
        }
        
        if (substring(old_name,1,1) == "t") {
            new_name <- paste(new_name, "Time", sep = "")
        }
        else new_name <- paste(new_name, "Freq", sep = "")
    
        if (substring(old_name,2,2) == "B") {
        new_name <- paste(new_name, "Body", sep = "")
        }
        else new_name <- paste(new_name, "Gravity", sep = "") 
        
        if(grepl("Acc", old_name)) {
            new_name <- paste(new_name, "Acc", sep = "")
        }
        
        if(grepl("Gyro", old_name)) {
            new_name <- paste(new_name, "Gyro", sep = "")
        }
        
        if(grepl("Jerk", old_name)) {
            new_name <- paste(new_name, "Jerk", sep = "")
        }
        
        if(grepl("Mag", old_name)) {
            new_name <- paste(new_name, "Magnitude", sep = "")
        }
        else{ 
            new_name <- paste(new_name, substring(old_name, nchar(old_name), nchar(old_name) ), sep = "")
        }
        
        
        colnames(data)[i+2] = new_name;
    }
    
    return(data)
}


###############################################################################
# Step 5: Creates a second, independent tidy data set with the average of each 
# variable for each activity and each subject.
###############################################################################

CreateNewDataset <- function(data) {
    
    new_data <- NULL
    
    data_by_subject <- split(data, data$Subject)
    
    for (a in 1:length(data_by_subject)){
        #new_data$Activity = new_data$Activity
        #data_by_activity_by_subject <- split(data_by_activity[[a]],as.factor(data$Subject))
        data_by_subject_by_activity <- group_by(data_by_subject[[a]],Activity)
        new_data <- rbind(new_data,summarise_each(data_by_subject_by_activity, funs(mean)))
        #print(new_data_rows)
    }
    return(group_by(new_data,Activity))
}


#####################
# Run all steps
#####################

#DownloadAndUnzip()

data <- MergeTrainAndTestData(1)
#data <- read.csv("./data/merged_data.csv")
#data
features <- GetFeatures() 

data <- FilterData(data,features)

data <- DescribeActivity(data)

data <- AddColumnNames(data, features)

tidy_data <- CreateNewDataset(data)

write.table(tidy_data, file = "./data/tidy_data.txt", row.name = FALSE)
