### This file describes hot the script run_analysis.R works.

 First, if not present, a folder name "data" is created under the current working directory.
 This is to separate the data from the script. Then, the dataset available at  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip  
 is dowloaded and unzipped into the data folder. 

 The Project description in the Course page is as follows:
 
 "You should create one R script called run_analysis.R that does the following. 
 1-Merges the training and the test sets to create one data set.
 2-Extracts only the measurements on the mean and standard deviation for each measurement. 
 3-Uses descriptive activity names to name the activities in the data set
 4-Appropriately labels the data set with descriptive variable names. 
 5-From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject".
 

 The script run_analysis.R is structured to execute the steps 1-5 in their natural order


###  Step 1:
  
  The train and test Activity labels are loaded from the source files train/y_train.txt and test/y_test.txt
  The train and test Subject labels are loaded from the source files train/subject_train.txt and test/subject_test.txt
  The train and test Features extracted from the Samsung measurements are loaded from the source files train/X_train.txt and test/X_test.txt
 The train and test data for Activity, Subject and Features are merged. Then all these data are combined in a single data frame, organized as follows:
 Column 1: Activity Column 2: Subject Column 3-563 Features (the name of this column set are not specified yet)

###  Step 2: 
  Looking at the file featurer.txt, containing all the Feature names, only the ones containing the words "mean()" or "std()" are selected. The corresponding indices are used to subset the### data frame (being careful because the data frame contains 2 more columns at the beginning) . A total of 66 feaures is selected, bringing the number of columns of the dataset down to 68 (Activity+Subject+selected Features)


###  Step 3:

  Looking the file activity_labels.txt, the element of the data frame column Activity are re-labeled using the activity names: WALKING, WALKING_UPSTAIRS, WALKING DOWNSTAIRS, SITTING, STANDING, LAYING.

###  Step 4:
  A new set of variable names is derived for the selected features. The new name are constructed from the original ones, with the goal of making them slightly more explicit. Please refer to the Codebook.md file for a description of the new names


###  Step 5:

  Using the "split", "group_by" and "summarise_each" function, a new independent tidy data set is created as required. The column names are the same as before, while the rows now contain the average of each feature for each activity and each subject. Since there are 6 unique activities and 30 total subjects, the final tidy_data.txt files contain 180 rows and 68 columns.
 
 

