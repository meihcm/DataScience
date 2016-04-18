library("data.table")
library("dplyr")
library("gdata")
projectHome <- paste("~/DataScience") ##"/Users/michaelchiem/DataScience"
datasetHome <- paste(projectHome,"/OnlineNewsPopularity",sep="")
setwd(datasetHome)
mashable_df = read.csv("OnlineNewsPopularity.csv", sep=",", header = TRUE)

