## SETUP
library("data.table")
library("dplyr")
library("gdata")
PROJECT_HOME <- paste("~/DataScience") ##"/Users/michaelchiem/DataScience"
DATASET_HOME <- paste(PROJECT_HOME,"/",sep="")

## 0: Load the data in RStudio
## Save the data set as a CSV file called titanic_original.csv and load it in RStudio into a data frame.
setwd(DATASET_HOME)
titanic_excel = read.xls("titanic3.xls", sheet = 1, header = TRUE)
write.csv(titanic_excel, file="titanic_original.csv")

## 1: Port of embarkation
## The embarked column has one missing value, which is known to correspond 
## to a passenger who actually embarked at Southampton. Find the missing value 
## and replace it with S.
titanic_df <- read.csv("titanic_original.csv", header=TRUE)
titanic_df <- data.frame(titanic_df)
titanic_df$embarked[trim(titanic_df$embarked) == ""] <- "S"

## 2: Age
## You’ll notice that a lot of the values in the Age column are missing. 
## While there are many ways to fill these missing values, using the mean 
## or median of the rest of the values is quite common in such cases.
## Calculate the mean of the Age column and use that value to populate 
## the missing values
mean_age <- mean(titanic_df$age, na.rm = TRUE)
mean_age <- round(mean_age)
titanic_df$age[is.na(titanic_df$age) ] <- mean_age

## Think about other ways you could have populated the missing values 
## in the age column. Why would you pick any of those over the mean (or not)?
## ANSWER: One might group by last name to assume a family group and then use average of those sub groups

## 3: Lifeboat
## You’re interested in looking at the distribution of passengers in 
## different lifeboats, but as we know, many passengers did not make it 
## to a boat :-( This means that there are a lot of missing values in the 
## boat column. Fill these empty slots with a dummy value e.g. NA
titanic_df$boat[trim(titanic_df$boat) == ""] <- NA                                                                                                                                     

## 4: Cabin
## You notice that many passengers don’t have a cabin number associated 
## with them.
## Does it make sense to fill missing cabin numbers with a value?
## ANSWER: No it may not make sense to have numbers assigned to cabin because they are more
## labels and not based on any ordinal values

## What does a missing value here mean?
## ANSWER: About 1014 of 1309 didn't have cabin designation in the column, and those 
## that didn't have cabins paid an average of 19.38, which suggests 
## they may not have cabins at such low fares.
## Highest fare was around $512.32

## You have a hunch that the fact that the cabin number is missing might 
## be a useful indicator of survival. Create a new column has_cabin_number 
## which has 1 if there is a cabin number, and 0 otherwise.
has_cabin_vector <- data.frame(has_cabin=1:nrow(titanic_df))
has_cabin_vector[has_cabin_vector > 0] <-0
titanic_df <- cbind(titanic_df, has_cabin_vector)
titanic_df$has_cabin[trim(titanic_df$cabin) != ""] <- 1

## 6: Submit the project on Github
## Include your code, the original data as a CSV file titanic_original.csv, 
## and the cleaned up data as a CSV file called titanic_clean.csv.
write.csv(titanic_df, file="titanic_clean.csv")