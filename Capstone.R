library("data.table")
library("dplyr")
library("gdata")
projectHome <- paste("~/DataScience") ##"/Users/michaelchiem/DataScience"
datasetHome <- paste(projectHome,"/OnlineNewsPopularity",sep="")
setwd(datasetHome)
mashable_df = read.csv("OnlineNewsPopularity.csv", sep=",", header = TRUE)
counter = 0;
for(thisUrl in mashable_df$url) 
{
  print(thisUrl)
  counter = counter + 1
  if (counter > 10)
    break
}

