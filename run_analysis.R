## SETUP
#### Assumes default is UCI Data home
PROJECT_HOME <- paste("~/DataScience") ##"/Users/michaelchiem/DataScience"
DATASET_HOME <- paste(PROJECT_HOME,"/UCI HAR Dataset",sep="")
TEST_HOME <- paste(PROJECT_HOME,"/UCI HAR Dataset/test",sep="")
TEST_INERTIAL_HOME <- paste(PROJECT_HOME,"/UCI HAR Dataset/test/Inertial Signals",sep="")
TRAIN_HOME <- paste(PROJECT_HOME,"/UCI HAR Dataset/train",sep="")
TRAIN_INERTIAL_HOME <- paste(PROJECT_HOME,"/UCI HAR Dataset/train/Inertial Signals",sep="")
## test 2
setwd(DATASET_HOME)
ACTIVITY_LABELS <- read.table("activity_labels.txt", col.names = c("activityKey","activityLabel"), header=FALSE)
FEATURE_MAP <- read.table("features.txt", header=FALSE)

## 1 Merge the training and test sets to create one dataset
#### 1.1 LOAD ALL TEST FILES
setwd(TEST_HOME)
TEST_SUBJECT_ROWS <- read.table("subject_test.txt", col.names = c("subjectKey"), header=FALSE)
TEST_Y_LABEL_ROWS <- read.table("y_test.txt", col.names=c("activityKey"), header=FALSE)
TEST_Y_LABEL_ROWS <- merge(x = TEST_Y_LABEL_ROWS, y = ACTIVITY_LABELS, by = "activityKey", all.x = TRUE)
TEST_Y_DATA <- read.table("X_test.txt", col.names=as.vector(FEATURE_MAP$V2), header=FALSE)

## TEST-Inertial-Acceleration merging (total x,y,z + body x,y,z)
setwd(TEST_INERTIAL_HOME)
COLUMN_VECTOR_HEADINGS <- c(1:128)
SENSOR_TYPE <- as.vector(rep(c("BODY_ACCEL_X"), each=nrow(TEST_SUBJECT_ROWS)))
TEST_INERTIAL_BODY_ACCEL_X <- read.table("body_acc_x_test.txt", col.names=as.vector(COLUMN_VECTOR_HEADINGS), header=FALSE)
TEST_INERTIAL_BODY_ACCEL_X <- cbind(TEST_SUBJECT_ROWS,TEST_Y_LABEL_ROWS,SENSOR_TYPE, TEST_INERTIAL_BODY_ACCEL_X)

SENSOR_TYPE <- as.vector(rep(c("BODY_ACCEL_Y"), each=nrow(TEST_SUBJECT_ROWS)))
TEST_INERTIAL_BODY_ACCEL_Y <- read.table("body_acc_y_test.txt", col.names=as.vector(COLUMN_VECTOR_HEADINGS), header=FALSE)
TEST_INERTIAL_BODY_ACCEL_Y <- cbind(TEST_SUBJECT_ROWS,TEST_Y_LABEL_ROWS,SENSOR_TYPE,TEST_INERTIAL_BODY_ACCEL_Y)

SENSOR_TYPE <- as.vector(rep(c("BODY_ACCEL_Z"), each=nrow(TEST_SUBJECT_ROWS)))
TEST_INERTIAL_BODY_ACCEL_Z <- read.table("body_acc_z_test.txt", col.names=as.vector(COLUMN_VECTOR_HEADINGS), header=FALSE)
TEST_INERTIAL_BODY_ACCEL_Z <- cbind(TEST_SUBJECT_ROWS,TEST_Y_LABEL_ROWS,SENSOR_TYPE,TEST_INERTIAL_BODY_ACCEL_Z)

SENSOR_TYPE <- as.vector(rep(c("TOTAL_ACCEL_X"), each=nrow(TEST_SUBJECT_ROWS)))
TEST_INERTIAL_TOTAL_ACCEL_X <- read.table("total_acc_x_test.txt", col.names=as.vector(COLUMN_VECTOR_HEADINGS), header=FALSE)
TEST_INERTIAL_TOTAL_ACCEL_X <- cbind(TEST_SUBJECT_ROWS,TEST_Y_LABEL_ROWS,SENSOR_TYPE,TEST_INERTIAL_TOTAL_ACCEL_X)

SENSOR_TYPE <- as.vector(rep(c("TOTAL_ACCEL_Y"), each=nrow(TEST_SUBJECT_ROWS)))
TEST_INERTIAL_TOTAL_ACCEL_Y <- read.table("total_acc_y_test.txt", col.names=as.vector(COLUMN_VECTOR_HEADINGS), header=FALSE)
TEST_INERTIAL_TOTAL_ACCEL_Y <- cbind(TEST_SUBJECT_ROWS,TEST_Y_LABEL_ROWS,SENSOR_TYPE,TEST_INERTIAL_TOTAL_ACCEL_Y)

SENSOR_TYPE <- as.vector(rep(c("TOTAL_ACCEL_Z"), each=nrow(TEST_SUBJECT_ROWS)))
TEST_INERTIAL_TOTAL_ACCEL_Z <- read.table("total_acc_z_test.txt", col.names=as.vector(COLUMN_VECTOR_HEADINGS), header=FALSE)
TEST_INERTIAL_TOTAL_ACCEL_Z <- cbind(TEST_SUBJECT_ROWS,TEST_Y_LABEL_ROWS,SENSOR_TYPE,TEST_INERTIAL_TOTAL_ACCEL_Z)

FULL_INERTIAL <- rbind(TEST_INERTIAL_BODY_ACCEL_X,TEST_INERTIAL_BODY_ACCEL_Y, TEST_INERTIAL_BODY_ACCEL_Z,TEST_INERTIAL_TOTAL_ACCEL_X,TEST_INERTIAL_TOTAL_ACCEL_Y,TEST_INERTIAL_TOTAL_ACCEL_Z)
## TEST-Inertial-Gyro merging (gyro x, y, z)


#### 1.2 LOAD ALL TRAIN FILES

## 2 Extract columns containing mean and std dev for each measurement

## 3 Create variable ActivityLabel and ActivityName that label all observations 
## with the corresponding activity labels and names respectively.

## 4 From the data set in step 3, create a second, independent tidy data set with 
## average of each variable for each activity and each subject.