## SETUP
#### Assumes default is UCI Data home
PROJECT_HOME <- paste("~/DataScience") ##"/Users/michaelchiem/DataScience"
DATASET_HOME <- paste(PROJECT_HOME,"/UCI HAR Dataset",sep="")
TEST_HOME <- paste(PROJECT_HOME,"/UCI HAR Dataset/test",sep="")
TRAIN_HOME <- paste(PROJECT_HOME,"/UCI HAR Dataset/train",sep="")
## test 2
setwd(DATASET_HOME)
ACTIVITY_LABELS <- read.table("activity_labels.txt", col.names = c("activityKey","activityLabel"), header=FALSE)
FEATURE_MAP <- read.table("features.txt", header=FALSE)

## 1 Merge the training and test sets to create one dataset
#### 1.1 LOAD ALL TEST FILES
setwd(TEST_HOME)
TEST_SUBJECT_ROWS <- read.table("subject_test.txt", col.names = c("subjectKey"), header=FALSE)
TEST_Y_LABEL_ROWS <- read.table("y_test.txt", col.names=c("labelKey"), header=FALSE)
TEST_Y_DATA <- read.table("X_test.txt", col.names=as.vector(T_FEATURE_MAP$V2), header=FALSE)
## ?? What is TEST_Y_DATA vs the inertial data??

#### 1.2 LOAD ALL TRAIN FILES

## 2 Extract columns containing mean and std dev for each measurement

## 3 Create variable ActivityLabel and ActivityName that label all observations 
## with the corresponding activity labels and names respectively.

## 4 From the data set in step 3, create a second, independent tidy data set with 
## average of each variable for each activity and each subject.