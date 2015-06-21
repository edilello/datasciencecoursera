###############################################################################
# This script implements the step required for the Project 1 of the 
# "Getting and Cleaning Data course"
#
# The daia is available at the following link: 
# 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 
# The script automatically downloads and unzip the data. Hovewer, the Project web-page 
# states that the script should work provided that the data is in the working directory
# 
# For further information about this script, please refer to the README.md file and the 
# code book Codebook.md
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


########################################################################
# Step1: Merge the training and the test sets to create one data set.
########################################################################


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

# Load features data : 7352/2947 rows 561 columns
feat_data_train <- read.table("./data/UCI\ HAR\ Dataset/train/X_train.txt", header = FALSE, stringsAsFactors = TRUE)
feat_data_test <- read.table("./data/UCI\ HAR\ Dataset/test/X_test.txt", header = FALSE, stringsAsFactors = TRUE)
# merge rows
feat_data_all <- rbind(feat_data_train, feat_data_test)


# Finally, merge these 3 data frames by columns : | Activity | Subject | Features
data <- cbind(activity_data_all, cbind(subject_data_all, feat_data_all))


################################################################################
# Find the names and the indices of the mean and standard deviation for each 
# measurement
################################################################################

# The names of the features are stored in the file "features.txt"
feature_names <- read.table("data/UCI\ HAR\ Dataset/features.txt", header = FALSE, as.is = TRUE)
feature_names <- feature_names$V2

# Find rows indices whose feature names contains either the words "mean()" or "std()"
feature_indices <- c(grep("mean()",feature_names,fixed = TRUE),grep("std()",feature_names,fixed = TRUE))    
feature_names <- feature_names[feature_indices]

# this indices will be used later on to change the corresponding column names in the merged data frame
# I have to shift them by "2" because the merged data frame also contains Activity and Subject columns
feature_indices <- feature_indices+2     


###############################################################################
# Step 2 : Extract only the measurements on the mean and standard deviation 
# for each measurement. 
###############################################################################

data <- data[,c(1,2,feature_indices)] 


####################################################################
# Step 3: replace activity values with descriptive activity names
####################################################################

activity_names <- read.table("data/UCI\ HAR\ Dataset/activity_labels.txt",header = FALSE)

# this is the original activity column data. Make it a factor
activity_label_fac <- as.factor(data$Activity)

# remap the levels of one factor into the other
new_activity_data <- plyr::mapvalues(activity_label_fac, levels(activity_label_fac), c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))

# put the new relabeled factor column in the original dataset
# (This could be made with a lot less code, but I am making each passage explicit for readability)
data[,"Activity"] <- new_activity_data

##############################################################################
# Step 4: Appropriately label the data set with descriptive variable names. 
##############################################################################

for (i in 1:length(feature_names)){
    
    # for the first columns, use original feature names, to obtain a more
    # decriptive and valid column name in R
    new_name  <- ""
    old_name <- feature_names[i]
    
    # Start the new column name with Mean or Std
    if(grepl("mean()", old_name)){
        new_name <- paste(new_name, "Mean", sep = "")
    }
    
    if(grepl("std()", old_name)) {
        new_name <- paste(new_name, "Std", sep = "")
    }
    
    # if the first letter of the old name starts with "t", the quantity
    # is Time-based, if it starts with "f" Frequency based 
    if (substring(old_name,1,1) == "t") {
        new_name <- paste(new_name, "Time", sep = "")
    }
    else new_name <- paste(new_name, "Freq", sep = "")
    
    # Some old names contain errors (such as repetition of "Body" in the name)
    # Fix this problem
    if (substring(old_name,2,2) == "B") {
        new_name <- paste(new_name, "Body", sep = "")
    }
    # old names refer wither to "Body" measurements or "Gravity" ones.
    else new_name <- paste(new_name, "Gravity", sep = "") 
    
    # Keep building the new colunn name incrementally
    if(grepl("Acc", old_name)) {
        new_name <- paste(new_name, "Acc", sep = "")
    }
    
    if(grepl("Gyro", old_name)) {
        new_name <- paste(new_name, "Gyro", sep = "")
    }
    
    if(grepl("Jerk", old_name)) {
        new_name <- paste(new_name, "Jerk", sep = "")
    }
    
    # The end of the old name is either "Mag", wich refers to
    # a magnitude of a vector, or the axis (XYX) to which the 
    # quantity refers to
    if(grepl("Mag", old_name)) {
        new_name <- paste(new_name, "Magnitude", sep = "")
    }
    else{ 
        new_name <- paste(new_name, substring(old_name, nchar(old_name), nchar(old_name) ), sep = "")
    }
    
    # remeber that the first two columns of data are "Activity" and "Subject"
    colnames(data)[i+2] = new_name;
}


###############################################################################
# Step 5: Creates a second, independent tidy data set with the average of each 
# variable for each activity and each subject.
###############################################################################

# empty frame, will be built incrementally
new_data <- NULL

# split data by activity
#data_by_activity <- split(data, data$Activity)

# split data by Subject
data_by_subject <- split(data, data$Subject)

# Use summarize_each to compute mean of each column after grouping by Activity
for (a in 1:length(data_by_subject)){
    
    #data_by_activity_by_subject <- group_by(data_by_activity[[a]],Subject)
    data_by_subject_by_activity <- group_by(data_by_subject[[a]],Activity)
    
    
    #new_data <- rbind(new_data,summarise_each(data_by_activity_by_subject, funs(mean)))
    new_data <- rbind(new_data,summarise_each(data_by_subject_by_activity, funs(mean)))
}

# I like the Subject to be the first column
new_data<-new_data[,c(2,1,3:68)]

# save the new data frame
write.table(new_data, file = "./data/tidy_data.txt", row.name = FALSE)

# ... and we're done! :-)

