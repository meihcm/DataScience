## SETUP
#### Assumes default is UCI Data home
PROJECT_HOME <- paste("~/DataScience") ##"/Users/michaelchiem/DataScience"
DATASET_HOME <- paste(PROJECT_HOME,"/UCI HAR Dataset",sep="")
TEST_HOME <- paste(PROJECT_HOME,"/UCI HAR Dataset/test",sep="")
TEST_INERTIAL_HOME <- paste(PROJECT_HOME,"/UCI HAR Dataset/test/Inertial Signals",sep="")
TRAIN_HOME <- paste(PROJECT_HOME,"/UCI HAR Dataset/train",sep="")
TRAIN_INERTIAL_HOME <- paste(PROJECT_HOME,"/UCI HAR Dataset/train/Inertial Signals",sep="")

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
TEST_Y_DATA <- cbind(TEST_SUBJECT_ROWS, TEST_Y_LABEL_ROWS, TEST_Y_DATA)

## TEST-Inertial-Acceleration merging (total x,y,z + body x,y,z)
setwd(TEST_INERTIAL_HOME)
COLUMN_VECTOR_HEADINGS <- paste("vector", 1:128, sep="")
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

FULL_TEST_ACCEL_INERTIAL <- rbind(TEST_INERTIAL_BODY_ACCEL_X,TEST_INERTIAL_BODY_ACCEL_Y, TEST_INERTIAL_BODY_ACCEL_Z,TEST_INERTIAL_TOTAL_ACCEL_X,TEST_INERTIAL_TOTAL_ACCEL_Y,TEST_INERTIAL_TOTAL_ACCEL_Z)
TEST_INERTIAL_BODY_ACCEL_X <- NULL
TEST_INERTIAL_BODY_ACCEL_Y <- NULL
TEST_INERTIAL_BODY_ACCEL_Z <- NULL
TEST_INERTIAL_TOTAL_ACCEL_X <- NULL
TEST_INERTIAL_TOTAL_ACCEL_Y <- NULL
TEST_INERTIAL_TOTAL_ACCEL_Z <- NULL

## TEST-Inertial-Gyro merging (gyro x, y, z)
SENSOR_TYPE <- as.vector(rep(c("BODY_GYRO_X"), each=nrow(TEST_SUBJECT_ROWS)))
TEST_INERTIAL_BODY_GYRO_X <- read.table("body_gyro_x_test.txt", col.names=as.vector(COLUMN_VECTOR_HEADINGS), header=FALSE)
TEST_INERTIAL_BODY_GYRO_X <- cbind(TEST_SUBJECT_ROWS,TEST_Y_LABEL_ROWS,SENSOR_TYPE,TEST_INERTIAL_BODY_GYRO_X)

SENSOR_TYPE <- as.vector(rep(c("BODY_GYRO_Y"), each=nrow(TEST_SUBJECT_ROWS)))
TEST_INERTIAL_BODY_GYRO_Y <- read.table("body_gyro_y_test.txt", col.names=as.vector(COLUMN_VECTOR_HEADINGS), header=FALSE)
TEST_INERTIAL_BODY_GYRO_Y <- cbind(TEST_SUBJECT_ROWS,TEST_Y_LABEL_ROWS,SENSOR_TYPE,TEST_INERTIAL_BODY_GYRO_Y)

SENSOR_TYPE <- as.vector(rep(c("BODY_GYRO_Z"), each=nrow(TEST_SUBJECT_ROWS)))
TEST_INERTIAL_BODY_GYRO_Z <- read.table("body_gyro_z_test.txt", col.names=as.vector(COLUMN_VECTOR_HEADINGS), header=FALSE)
TEST_INERTIAL_BODY_GYRO_Z <- cbind(TEST_SUBJECT_ROWS,TEST_Y_LABEL_ROWS,SENSOR_TYPE,TEST_INERTIAL_BODY_GYRO_Z)
FULL_TEST_GYRO_INERTIAL <- rbind(TEST_INERTIAL_BODY_GYRO_X,TEST_INERTIAL_BODY_GYRO_Y, TEST_INERTIAL_BODY_GYRO_Z)
TEST_INERTIAL_BODY_GYRO_X <- NULL
TEST_INERTIAL_BODY_GYRO_Y <- NULL
TEST_INERTIAL_BODY_GYRO_Z <- NULL

#### 1.2 LOAD ALL TRAIN FILES
setwd(TRAIN_HOME)
TRAIN_SUBJECT_ROWS <- read.table("subject_train.txt", col.names = c("subjectKey"), header=FALSE)
TRAIN_Y_LABEL_ROWS <- read.table("y_train.txt", col.names=c("activityKey"), header=FALSE)
TRAIN_Y_LABEL_ROWS <- merge(x = TRAIN_Y_LABEL_ROWS, y = ACTIVITY_LABELS, by = "activityKey", all.x = TRUE)
TRAIN_Y_DATA <- read.table("X_train.txt", col.names=as.vector(FEATURE_MAP$V2), header=FALSE)
TRAIN_Y_DATA <- cbind(TRAIN_SUBJECT_ROWS, TRAIN_Y_LABEL_ROWS, TRAIN_Y_DATA)

## TEST-Inertial-Acceleration merging (total x,y,z + body x,y,z)
setwd(TRAIN_INERTIAL_HOME)
SENSOR_TYPE <- as.vector(rep(c("BODY_ACCEL_X"), each=nrow(TRAIN_SUBJECT_ROWS)))
TRAIN_INERTIAL_BODY_ACCEL_X <- read.table("body_acc_x_train.txt", col.names=as.vector(COLUMN_VECTOR_HEADINGS), header=FALSE)
TRAIN_INERTIAL_BODY_ACCEL_X <- cbind(TRAIN_SUBJECT_ROWS,TRAIN_Y_LABEL_ROWS,SENSOR_TYPE, TRAIN_INERTIAL_BODY_ACCEL_X)

SENSOR_TYPE <- as.vector(rep(c("BODY_ACCEL_Y"), each=nrow(TRAIN_SUBJECT_ROWS)))
TRAIN_INERTIAL_BODY_ACCEL_Y <- read.table("body_acc_y_train.txt", col.names=as.vector(COLUMN_VECTOR_HEADINGS), header=FALSE)
TRAIN_INERTIAL_BODY_ACCEL_Y <- cbind(TRAIN_SUBJECT_ROWS,TRAIN_Y_LABEL_ROWS,SENSOR_TYPE, TRAIN_INERTIAL_BODY_ACCEL_Y)

SENSOR_TYPE <- as.vector(rep(c("BODY_ACCEL_Z"), each=nrow(TRAIN_SUBJECT_ROWS)))
TRAIN_INERTIAL_BODY_ACCEL_Z <- read.table("body_acc_z_train.txt", col.names=as.vector(COLUMN_VECTOR_HEADINGS), header=FALSE)
TRAIN_INERTIAL_BODY_ACCEL_Z <- cbind(TRAIN_SUBJECT_ROWS,TRAIN_Y_LABEL_ROWS,SENSOR_TYPE, TRAIN_INERTIAL_BODY_ACCEL_Z)

SENSOR_TYPE <- as.vector(rep(c("TOTAL_ACCEL_X"), each=nrow(TRAIN_SUBJECT_ROWS)))
TRAIN_INERTIAL_TOTAL_ACCEL_X <- read.table("total_acc_x_train.txt", col.names=as.vector(COLUMN_VECTOR_HEADINGS), header=FALSE)
TRAIN_INERTIAL_TOTAL_ACCEL_X <- cbind(TRAIN_SUBJECT_ROWS,TRAIN_Y_LABEL_ROWS,SENSOR_TYPE,TRAIN_INERTIAL_TOTAL_ACCEL_X)

SENSOR_TYPE <- as.vector(rep(c("TOTAL_ACCEL_Y"), each=nrow(TRAIN_SUBJECT_ROWS)))
TRAIN_INERTIAL_TOTAL_ACCEL_Y <- read.table("total_acc_y_train.txt", col.names=as.vector(COLUMN_VECTOR_HEADINGS), header=FALSE)
TRAIN_INERTIAL_TOTAL_ACCEL_Y <- cbind(TRAIN_SUBJECT_ROWS,TRAIN_Y_LABEL_ROWS,SENSOR_TYPE,TRAIN_INERTIAL_TOTAL_ACCEL_Y)

SENSOR_TYPE <- as.vector(rep(c("TOTAL_ACCEL_Z"), each=nrow(TRAIN_SUBJECT_ROWS)))
TRAIN_INERTIAL_TOTAL_ACCEL_Z <- read.table("total_acc_z_train.txt", col.names=as.vector(COLUMN_VECTOR_HEADINGS), header=FALSE)
TRAIN_INERTIAL_TOTAL_ACCEL_Z <- cbind(TRAIN_SUBJECT_ROWS,TRAIN_Y_LABEL_ROWS,SENSOR_TYPE,TRAIN_INERTIAL_TOTAL_ACCEL_Z)

FULL_TRAIN_ACCEL_INERTIAL <- rbind(TRAIN_INERTIAL_BODY_ACCEL_X,TRAIN_INERTIAL_BODY_ACCEL_Y, TRAIN_INERTIAL_BODY_ACCEL_Z,TRAIN_INERTIAL_TOTAL_ACCEL_X,TRAIN_INERTIAL_TOTAL_ACCEL_Y,TRAIN_INERTIAL_TOTAL_ACCEL_Z)
TRAIN_INERTIAL_BODY_ACCEL_X <- NULL
TRAIN_INERTIAL_BODY_ACCEL_Y <- NULL
TRAIN_INERTIAL_BODY_ACCEL_Z <- NULL
TRAIN_INERTIAL_TOTAL_ACCEL_X <- NULL
TRAIN_INERTIAL_TOTAL_ACCEL_Y <- NULL
TRAIN_INERTIAL_TOTAL_ACCEL_Z <- NULL

## TRAIN-Inertial-Gyro merging (gyro x, y, z)
SENSOR_TYPE <- as.vector(rep(c("BODY_GYRO_X"), each=nrow(TRAIN_SUBJECT_ROWS)))
TRAIN_INERTIAL_BODY_GYRO_X <- read.table("body_gyro_x_train.txt", col.names=as.vector(COLUMN_VECTOR_HEADINGS), header=FALSE)
TRAIN_INERTIAL_BODY_GYRO_X <- cbind(TRAIN_SUBJECT_ROWS,TRAIN_Y_LABEL_ROWS,SENSOR_TYPE,TRAIN_INERTIAL_BODY_GYRO_X)

SENSOR_TYPE <- as.vector(rep(c("BODY_GYRO_Y"), each=nrow(TRAIN_SUBJECT_ROWS)))
TRAIN_INERTIAL_BODY_GYRO_Y <- read.table("body_gyro_y_train.txt", col.names=as.vector(COLUMN_VECTOR_HEADINGS), header=FALSE)
TRAIN_INERTIAL_BODY_GYRO_Y <- cbind(TRAIN_SUBJECT_ROWS,TRAIN_Y_LABEL_ROWS,SENSOR_TYPE,TRAIN_INERTIAL_BODY_GYRO_Y)

SENSOR_TYPE <- as.vector(rep(c("BODY_GYRO_Z"), each=nrow(TRAIN_SUBJECT_ROWS)))
TRAIN_INERTIAL_BODY_GYRO_Z <- read.table("body_gyro_z_train.txt", col.names=as.vector(COLUMN_VECTOR_HEADINGS), header=FALSE)
TRAIN_INERTIAL_BODY_GYRO_Z <- cbind(TRAIN_SUBJECT_ROWS,TRAIN_Y_LABEL_ROWS,SENSOR_TYPE,TRAIN_INERTIAL_BODY_GYRO_Z)
FULL_TRAIN_GYRO_INERTIAL <- rbind(TRAIN_INERTIAL_BODY_GYRO_X,TRAIN_INERTIAL_BODY_GYRO_Y, TRAIN_INERTIAL_BODY_GYRO_Z)
TRAIN_INERTIAL_BODY_GYRO_X <- NULL
TRAIN_INERTIAL_BODY_GYRO_Y <- NULL
TRAIN_INERTIAL_BODY_GYRO_Z <- NULL

## Merge test/train data
FULL_TABLE <- rbind(TEST_Y_DATA, TRAIN_Y_DATA)
## Merge Inertial data
FULL_INERTIAL_TABLE <- rbind(FULL_TEST_ACCEL_INERTIAL,FULL_TEST_GYRO_INERTIAL, FULL_TRAIN_ACCEL_INERTIAL,FULL_TRAIN_GYRO_INERTIAL)

## 2 Extract columns containing mean and std dev for each measurement
FULL_STD_TABLE <- FULL_TABLE[grepl("std", tolower(names(FULL_TABLE)))]
FULL_AVG_TABLE <- FULL_TABLE[grepl("mean", tolower(names(FULL_TABLE)))]
## and 3 Create variable ActivityLabel and ActivityName that label all observations 
## with the corresponding activity labels and names respectively.
NEW_COLUMN_NAMES <- c("subjectKey","activityKey", "activityLabel", names(FULL_STD_TABLE), names(FULL_AVG_TABLE))
FULL_ANALYSIS_TABLE <- cbind(FULL_TABLE$subjectKey,FULL_TABLE$activityKey, FULL_TABLE$activityLabel, FULL_STD_TABLE, FULL_AVG_TABLE)
names(FULL_ANALYSIS_TABLE) <- NEW_COLUMN_NAMES
FULL_STD_TABLE <- NULL
FULL_AVG_TABLE <- NULL

## 4 From the data set in step 3, create a second, independent tidy data set with 
## average of each variable for each activity and each subject.