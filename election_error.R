install.packages("ggplot2")
library(ggplot2)
library(plyr)
library(dplyr)
require("caret")
library(caret)
library(caTools)
library(gridExtra)
require("psych")
library(psych)
library(data.table)

## Be careful remove all variables ##
rm(list = ls())
setwd("/Users/michaelchiem/DataScience")
############################################## Doing full parse from raw file
## Parse errors
## NBC
errors <- read.table(pipe("grep 'Failed Save' nbc_elections.log.2016_presidential"), sep='|',quote="\"")
errors$Time <- errors$V1
errors$Thread <-errors$V1
errors$RaceId <-errors$V1
errors$Station <- errors$V1
if(!exists("errors")) {
  errors <- data.frame(
                   Day=character(),
                   Time=character(),
                   Thread=character(),
                   RaceId=character(),
                   Station=character(),
                   stringsAsFactors=FALSE)
}
names(errors) <- c("Day", "Time", "Thread", "RaceId", "Station")

a <- errors
## Clean for stations
a$Station <- gsub('^.*raceId=elections2016-',"",a$Station)
a$Station <- gsub('-.*$',"",a$Station)

## Clean for RaceId
a$RaceId <- gsub('^.*raceId=',"",a$RaceId)
a$RaceId <- gsub(',.*$',"",a$RaceId)

## Clean for Thread
a$Thread <- gsub('^.*Thread-',"",a$Thread)
a$Thread <- gsub('].*$',"",a$Thread)

## Clean for Time
a$Time <- gsub(' ERROR.*$',"",a$Time)
a$Time <- gsub('\\[.* ',"",a$Time)

## Clean for Day
a$Day <- gsub(' .*$',"",a$Day)
a$Day <- gsub('\\[',"",a$Day)

errors <- a
nbc_errors <- errors

## TLM
errors <- read.table(pipe("grep 'Failed Save' tlm_elections.log.2016_presidential"), sep='|',quote="\"")
errors$Time <- errors$V1
errors$Thread <-errors$V1
errors$RaceId <-errors$V1
errors$Station <- errors$V1
if(!exists("errors")) {
  errors <- data.frame(
    Day=character(),
    Time=character(),
    Thread=character(),
    RaceId=character(),
    Station=character(),
    stringsAsFactors=FALSE)
}
names(errors) <- c("Day", "Time", "Thread", "RaceId", "Station")

a <- errors
## Clean for stations
a$Station <- gsub('^.*raceId=elections2016-',"",a$Station)
a$Station <- gsub('-.*$',"",a$Station)

## Clean for RaceId
a$RaceId <- gsub('^.*raceId=',"",a$RaceId)
a$RaceId <- gsub(',.*$',"",a$RaceId)

## Clean for Thread
a$Thread <- gsub('^.*Thread-',"",a$Thread)
a$Thread <- gsub('].*$',"",a$Thread)

## Clean for Time
a$Time <- gsub(' ERROR.*$',"",a$Time)
a$Time <- gsub('\\[.* ',"",a$Time)

## Clean for Day
a$Day <- gsub(' .*$',"",a$Day)
a$Day <- gsub('\\[',"",a$Day)

errors <- rbind(nbc_errors,a)

## Parse successes
## nbc
successes <- read.table(pipe("grep 'Success Save' nbc_elections.log.2016_presidential"), sep='|', quote="\"")
successes$Time <- successes$V1
successes$Thread <-successes$V1
successes$RaceId <-successes$V1
successes$Station <- successes$V1
names(successes) <- c("Day", "Time", "Thread", "RaceId", "Station")
a <- successes
## For station
a$Station <- gsub('^.*raceId=elections2016-',"",a$Station)
a$Station <- gsub('-.*$',"",a$Station)

## Clean for RaceId
a$RaceId <- gsub('^.*raceId=',"",a$RaceId)
a$RaceId <- gsub(',.*$',"",a$RaceId)

## Clean for Thread
a$Thread <- gsub('^.*Thread-',"",a$Thread)
a$Thread <- gsub('].*$',"",a$Thread)

## Clean for Time
a$Time <- gsub(' INFO.*$',"",a$Time)
a$Time <- gsub('\\[.* ',"",a$Time)

## Clean for Day
a$Day <- gsub(' .*$',"",a$Day)
a$Day <- gsub('\\[',"",a$Day)

successes <- a
nbc_successes <- successes

## TLM
successes <- read.table(pipe("grep 'Success Save' tlm_elections.log.2016_presidential"), sep='|', quote="\"")
successes$Time <- successes$V1
successes$Thread <-successes$V1
successes$RaceId <-successes$V1
successes$Station <- successes$V1
names(successes) <- c("Day", "Time", "Thread", "RaceId", "Station")
a <- successes
## For station
a$Station <- gsub('^.*raceId=elections2016-',"",a$Station)
a$Station <- gsub('-.*$',"",a$Station)

## Clean for RaceId
a$RaceId <- gsub('^.*raceId=',"",a$RaceId)
a$RaceId <- gsub(',.*$',"",a$RaceId)

## Clean for Thread
a$Thread <- gsub('^.*Thread-',"",a$Thread)
a$Thread <- gsub('].*$',"",a$Thread)

## Clean for Time
a$Time <- gsub(' INFO.*$',"",a$Time)
a$Time <- gsub('\\[.* ',"",a$Time)

## Clean for Day
a$Day <- gsub(' .*$',"",a$Day)
a$Day <- gsub('\\[',"",a$Day)

successes <- rbind(nbc_successes,a)

############################################## End doing full parse from raw file

## Start with failure df
##failed_df <- read.csv("out.csv",header=TRUE,sep=",")
failed_df <- errors
tempDate <- as.POSIXct( paste(failed_df$Day," ",failed_df$Time, sep=""), format="%m-%d %H:%M", tz="America/New_York")

## Convert from UTC to EDT
##tempDate <- as.POSIXct( tempDate, "America/Los_Angeles")
failed_df$convertedEDT <- tempDate
attr(failed_df$convertedEDT, "tzone") <- "America/New_York"
failed_df$convertedEDTh <- as.numeric(format(failed_df$convertedEDT,"%H")) * 60
failed_df$convertedEDThm <- failed_df$convertedEDTh + as.numeric(format(failed_df$convertedEDT,"%M"))
failed_df$convertedEDTDay = format(failed_df$convertedEDT,format="%d-%B")
failed_df$convertedEDTh <-  failed_df$convertedEDTh/60
failed_df$RaceId <- as.character(failed_df$RaceId)
failed_df$status = "fail"

## Run some binning for five minute and 2 minute intervals
failed_df$five_min_bin = floor((failed_df$convertedEDThm + 1)/ 5)
failed_df$two_min_bin = floor((failed_df$convertedEDThm + 1)/ 2)

## Success df
##success_df <- read.csv("success.csv",header=TRUE,sep=",")
success_df <- successes
tempDate <- as.POSIXct( paste(success_df$Day," ",success_df$Time, sep=""), format="%m-%d %H:%M", tz="America/New_York")

## Convert from UTC to EDT
##tempDate <- as.POSIXct( tempDate, "America/Los_Angeles")
success_df$convertedEDT <- tempDate
attr(success_df$convertedEDT, "tzone") <- "America/New_York"
success_df$convertedEDTh <- as.numeric(format(success_df$convertedEDT,"%H")) * 60
success_df$convertedEDThm <- success_df$convertedEDTh + as.numeric(format(success_df$convertedEDT,"%M"))
success_df$convertedEDTDay = format(success_df$convertedEDT,format="%d-%B")
success_df$convertedEDTh <-  success_df$convertedEDTh/60
success_df$RaceId <- as.character(success_df$RaceId)
success_df$status = "success"

## Run some binning for five minute and 2 minute intervals
success_df$five_min_bin = floor((success_df$convertedEDThm + 1)/ 5)
success_df$two_min_bin = floor((success_df$convertedEDThm + 1)/ 2)

## Removing duplicate successes that failed within a minute of each other
#keys <- c("RaceId", "five_min_bin")
#tData <- data.table(success_df, key=keys)
#tBounce <- data.table(failed_df, key=keys)
#tData[tBounce, Bounced := 1L]
#tData <- tData[is.na(tData$Bounced),]
#tData$Bounced <- NULL

## Bind the two df
out_df = rbind(failed_df,success_df)

numOfFiveBins <- (1 + max(out_df$convertedEDTh) - min(out_df$convertedEDTh)) * 12

## Add a factor to represent 1 to 10 for every 10 minute interval (within a single day)
## Used to see which minute that the api limit refreshes
out_df$convertedTenMinCycle <- out_df$convertedEDThm %% 60 %% 10
out_df$convertedFiveMinCycle <- out_df$convertedEDThm %% 60 %% 5

## Calculating ratios within columns based on groupings
out_df$five_min_bin_as_percent_of_day_trans = 0

## group by both day and five min bin to get an error rate
out_df = out_df %>% group_by(convertedEDTDay) %>% mutate(total_transaction_for_day = length(status))

## group by both day and five min bin to get an error rate
out_df = out_df %>% group_by(convertedEDTDay,five_min_bin) %>% mutate(five_min_bin_weighted_failure_rate = sum(status == "fail") / total_transaction_for_day)

## group by both day and five min bin to get an error rate
out_df = out_df %>% group_by(convertedEDTDay,five_min_bin) %>% mutate(five_min_bin_failure_rate = (sum(status == "fail") / length(status)))

## group by RaceId to take average of five_min_bin_failure_rate across time
out_df = out_df %>% group_by(RaceId) %>% mutate(all_time_race_failure_rate_average = mean(five_min_bin_failure_rate))

## remove duplicates to get top 10 race id with failures, sort and get top ten
top_ten_errant_race = aggregate(out_df,by=list(out_df$RaceId,out_df$all_time_race_failure_rate_average),FUN=length)
names(top_ten_errant_race) = c("RaceId", "all_time_race_failure_rate_average")
top_ten_errant_race = top_ten_errant_race[,1:2]
## Sort
top_ten_errant_race <- top_ten_errant_race[order(-top_ten_errant_race$all_time_race_failure_rate_average),] 
top_ten_errant_race <- top_ten_errant_race[1:10,] 
## plot it connecting points with line
top_ten_errant_race$RaceId = factor(top_ten_errant_race$RaceId, levels = top_ten_errant_race$RaceId[order(top_ten_errant_race$all_time_race_failure_rate_average)])
##XXggplot(top_ten_errant_race, aes(x=RaceId,y=all_time_race_failure_rate_average,colour=RaceId,fill=RaceId)) + 
  ##geom_bar(stat = "identity",width=0.4) + coord_flip() + xlab("Top Race Id") + ylab("Average Error")

## Box plotting top_ten across the 4 days
box_top_ten_errant_race = out_df[out_df$RaceId %in% top_ten_errant_race$RaceId,]
box_top_ten_errant_race = aggregate(box_top_ten_errant_race,by=list(box_top_ten_errant_race$RaceId,box_top_ten_errant_race$five_min_bin_failure_rate, box_top_ten_errant_race$convertedEDTDay),FUN=length)
names(box_top_ten_errant_race) = c("RaceId", "five_min_bin_failure_rate","convertedEDTDay")
box_top_ten_errant_race = box_top_ten_errant_race[,1:3]
##XXggplot(data = box_top_ten_errant_race, aes(x=convertedEDTDay, y=five_min_bin_failure_rate)) + 
##XX  geom_boxplot(aes(fill=convertedEDTDay)) +
##XX  stat_boxplot(geom ='errorbar') + 
##XX  xlab("Day") +
##XX  ylab("Failure Rate Observations (5 Min Intervals)") +
##XX  labs(fill = "Day") +
##XX  facet_wrap( ~ RaceId, scales="free")

## Box plotting all races across the 4 days
box_all_errant_race = out_df
box_all_errant_race = aggregate(box_all_errant_race,by=list(box_all_errant_race$RaceId,box_all_errant_race$five_min_bin_failure_rate, box_all_errant_race$convertedEDTDay),FUN=length)
names(box_all_errant_race) = c("RaceId", "five_min_bin_failure_rate","convertedEDTDay")
box_all_errant_race = box_all_errant_race[,1:3]
##XXggplot(data = box_all_errant_race, aes(x=convertedEDTDay, y=five_min_bin_failure_rate)) + 
##XX  geom_boxplot(aes(fill=convertedEDTDay)) +
##XX  stat_boxplot(geom ='errorbar') + 
##XX  xlab("Day") +
##XX  ylab("Failure Rate Observations (5 Min Intervals)") +
##XX  labs(fill = "Day") +
##XX  facet_wrap( ~ convertedEDTDay, scales="free")

## Plotting top_ten on June 10th by hours
box_top_ten_errant_race_june_10 = out_df[out_df$RaceId %in% top_ten_errant_race$RaceId & out_df$convertedEDTDay=="10-June",]
box_top_ten_errant_race_june_10 = aggregate(box_top_ten_errant_race_june_10,by=list(box_top_ten_errant_race_june_10$RaceId,box_top_ten_errant_race_june_10$convertedEDTh, box_top_ten_errant_race_june_10$convertedEDThm,box_top_ten_errant_race_june_10$five_min_bin_failure_rate),FUN=length)
names(box_top_ten_errant_race_june_10) = c("RaceId", "convertedEDTh","convertedEDThm","five_min_bin_failure_rate")
box_top_ten_errant_race_june_10 = box_top_ten_errant_race_june_10[,1:4]
box_top_ten_errant_race_june_10 = aggregate(box_top_ten_errant_race_june_10,by=list(box_top_ten_errant_race_june_10$RaceId,box_top_ten_errant_race_june_10$convertedEDTh),FUN=mean)
box_top_ten_errant_race_june_10 = box_top_ten_errant_race_june_10[,c(1,2,6)]
names(box_top_ten_errant_race_june_10) = c("RaceId", "convertedEDTh","avg_five_min_bin_failure_rate")
##XXggplot(data = box_top_ten_errant_race_june_10, aes(x=convertedEDTh, y=avg_five_min_bin_failure_rate)) + 
##XX  geom_line(aes()) +
##XX  xlab("Hour") +
##XX  ylab("Average Failure Rate") +
##XX  facet_wrap( ~ RaceId, scales="free")


## General plots
## Full errors
##XXggplot(data=subset(out_df, status=="fail"),mapping=aes(x=(convertedEDThm/60))) + 
##XX  geom_histogram(bins=numOfFiveBins, color="red") + 
##XX  facet_wrap(~convertedEDTDay) + 
##XX  xlab("Time of Day (EDT)") + 
##XX  geom_hline(yintercept=500,linetype="dashed",color="red")

## Full successes
##XXggplot(data=subset(out_df, status=="success"),mapping=aes(x=(convertedEDThm/60))) + geom_histogram(bins=numOfFiveBins, color="cyan3") + 
##XX  facet_wrap(~convertedEDTDay) + 
##XX  xlab("Time of Day (EDT)") + 
##XX  geom_hline(yintercept=500,linetype="dashed",color="red") 
## Help with facet ordering
out_df$convertedEDTDayAsDate <- as.Date(out_df$convertedEDTDay, "%d-%B")
prettyPrintFacetNames <- function(string) {
  return(format(string, format="%B-%d"))
}
## 2016-11-08 17:17:00
## startDate = as.POSIXct("2016-11-08 17:00:00", "%y-%m-%d %H:%M:%S")
## attr(startDate, "tzone") <- "America/New_York"
## endDate <- as.POSIXct("2016-11-09 17:00:00", "%y-%m-%d %H:%M:%S")
## attr(endDate, "tzone") <- "America/New_York"

## Combined success and failure on one graph
ggplot(out_df, aes(convertedEDThm/60)) +
  geom_freqpoly(aes(group = status, colour = status), bins=numOfFiveBins) + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(limits=c(1,3000), expand=c(0,0))+
  facet_wrap(~convertedEDTDayAsDate) +
  xlab("Time of Day (ET)")  +
  ylab("Frequency of Updates (5 Minute Intervals)")
  ## geom_hline(yintercept=500,linetype="dashed",color="red") 

## Using stat_count type of plotting
## Group by count per hour
group_by_hour_count_df = aggregate(out_df,by=list(out_df$convertedEDTDayAsDate,out_df$convertedEDTh),FUN=length)
names(group_by_hour_count_df) = c("convertedEDTDayAsDate","convertedEDTh","count")
group_by_hour_count_df <- group_by_hour_count_df[,1:3]

ggplot(group_by_hour_count_df, aes(convertedEDTh, count)) +
  geom_point() +
  facet_wrap(~convertedEDTDayAsDate) +
  xlab("Time of Day (ET)") +
  ylab("Total Per Hour") +
  geom_line()


## Combined failure rate
grouped_failure_rate = aggregate(out_df,by=list(out_df$five_min_bin,out_df$five_min_bin_failure_rate,out_df$convertedEDTDay),FUN=length)
names(grouped_failure_rate) = c("five_min_bin", "five_min_bin_failure_rate","convertedEDTDay","count")
## Plot it
LtoM <-colorRampPalette(c('red', 'yellow' ))
Mid <- "snow3"
MtoH <-colorRampPalette(c('orange', 'red'))
##XXggplot(grouped_failure_rate, aes(x=five_min_bin * 5/60,y=five_min_bin_failure_rate,fill=five_min_bin_failure_rate)) +
##XX  geom_bar(stat="identity") + facet_wrap(~convertedEDTDay) +
##XX  xlab("Time of Day (EDT)") +
##XX  ylab("Error Rate (Interval Error / Full Day Transaction)") +
##XX  labs(fill="Error Rate") +
##XX  scale_fill_gradient2(low=LtoM(100), mid='snow3', 
##XX                       high=MtoH(100), space='Lab')

## Analyze closer date range with close to 500 transactions per 5 minute bins
## Closer look at problem
## Find peak and times 
## five min interval
high_trans_period_df <- aggregate(out_df, by=list(out_df$convertedEDTDay, out_df$five_min_bin), FUN=length)
high_trans_period_df <- high_trans_period_df[,1:3]
names(high_trans_period_df) = c("convertedEDTDay","five_min_bin","count_of_trans")
high_trans_period_df <- high_trans_period_df[high_trans_period_df$count_of_trans >= 490,]
high_trans_period_df <- high_trans_period_df[order(high_trans_period_df$convertedEDTDay,high_trans_period_df$five_min_bin,decreasing=TRUE),] 
end_day <- high_trans_period_df[1,]$convertedEDTDay
end_time <- floor(high_trans_period_df[1,]$five_min_bin * 1/12)
start_day <- high_trans_period_df[nrow(high_trans_period_df),]$convertedEDTDay
start_time <- floor(high_trans_period_df[nrow(high_trans_period_df),]$five_min_bin * 1/12)
#** MUST RUN THESE IF YOU WANT BELOW PLOTS ** 
problem_df <- out_df[out_df$convertedEDTDay == start_day,]
problem_df <- problem_df[problem_df$convertedEDTh >=start_time,]
problem_df2 <- out_df[out_df$convertedEDTDay == end_day,]
problem_df2 <- problem_df2[problem_df2$convertedEDTh <= end_time,]
problem_df <- rbind(problem_df, problem_df2)
numOfFiveBins <- (1 + max(problem_df$convertedEDTh) - min(problem_df$convertedEDTh)) * 12

## bins may not be 144!! 
##ggplot(out_df, aes(convertedEDT)) +
##  geom_freqpoly(aes(group = status, colour = status), bins=numOfFiveBins) +
##  xlab("Time of Day (EDT)") + 
##  geom_hline(yintercept=500,linetype="dashed",color="red") 

##XXggplot(out_df, aes(convertedEDT)) +
##XX  geom_freqpoly(aes(group = status, colour = status), bins=numOfFiveBins) +
##XX  xlab("Time of Day (EDT)") + 
##XX  geom_hline(yintercept=500,linetype="dashed",color="red") 

## See if the 10 minute interval cycle visual has any hint of when the api resets from the problem_df dataset
## See if the x axis is ten minute cycle has more clues
##XXggplot(problem_df, aes(convertedTenMinCycle)) +
##XX  geom_bar(aes(group = status, colour = status, fill=status)) +
##XX  xlab("10 Minute Cycles") + 
##XX  facet_wrap(~Station) +
##XX  geom_hline(yintercept=500,linetype="dashed",color="red") 

## Now expand it to all data
##XXggplot(out_df, aes(convertedTenMinCycle)) +
##XX  geom_bar(aes(group = status, colour = status, fill=status)) +
##XX  xlab("10 Minute Cycles") + 
##XX  facet_wrap(~Station) +
##XX  geom_hline(yintercept=500,linetype="dashed",color="red") 

## Only failures of problem_df
problem_fail_only_df <- subset(problem_df, status=="fail")
five_min_interval_problem_df <- aggregate(problem_fail_only_df, by=list(problem_fail_only_df$Station, problem_fail_only_df$convertedFiveMinCycle), FUN=length)
names(five_min_interval_problem_df) = c("Station","convertedFiveMinCycle","Count")
five_min_interval_problem_df <- five_min_interval_problem_df[,1:3]
## Now plot it
##XXggplot(five_min_interval_problem_df,aes(x=convertedFiveMinCycle,y=Count)) + geom_bar(stat="identity") + facet_wrap(~Station)

## Now expand it to across all data with failures
only_failed_df = subset(out_df, status=="fail")
five_min_interval_all_df <- aggregate(only_failed_df, by=list(only_failed_df$Station, only_failed_df$convertedFiveMinCycle), FUN=length)
names(five_min_interval_all_df) = c("Station","convertedFiveMinCycle","Count")
five_min_interval_all_df <- five_min_interval_all_df[,1:3]
## Now plot it
##XXggplot(five_min_interval_all_df,aes(x=convertedFiveMinCycle,y=Count)) + geom_bar(stat="identity") + facet_wrap(~Station)

## How bout map successes and not failures
only_success_df = subset(out_df, status=="success")
five_min_interval_all_df <- aggregate(only_success_df, by=list(only_success_df$Station, only_success_df$convertedFiveMinCycle), FUN=length)
names(five_min_interval_all_df) = c("Station","convertedFiveMinCycle","Count")
five_min_interval_all_df <- five_min_interval_all_df[,1:3]
## Now plot it
ggplot(five_min_interval_all_df,aes(x=convertedFiveMinCycle,y=(Count))) + geom_bar(stat="identity")  +
  xlab("Each Minute of 5 Minute Interval") + ylab("Total Saves") + facet_wrap(~Station)

## Over time success and failures by station (all station/all day)
##XXggplot(out_df, aes(convertedEDT)) +
##XX  geom_freqpoly(aes(group = status, colour = status), bins=numOfFiveBins) + facet_wrap(~Station) +
##XX  xlab("Time of Day (EDT)") + 
##XX  geom_hline(yintercept=500,linetype="dashed",color="red") 

## Do some aggregations
count_of_unique_by_station <- aggregate(out_df, by=list(out_df$Station, out_df$RaceId), FUN=length);
count_of_unique_by_station <- count_of_unique_by_station[,1:3]
names(count_of_unique_by_station) = c("Station", "RaceId", "count")

#### Unique races by Station
count_of_unique_races_by_station <- aggregate(RaceId~Station,count_of_unique_by_station,length);
names(count_of_unique_races_by_station) = c("Station","Count")
## Sort by small to large
count_of_unique_races_by_station$Station <- factor(count_of_unique_races_by_station$Station, levels = count_of_unique_races_by_station$Station[order(count_of_unique_races_by_station$Count)])
## Plot it
ggplot(count_of_unique_races_by_station, aes(x=Station,y=Count,colour=Station,fill=Station)) + geom_bar(stat = "identity") +
  ylab("Count of Unique RaceId Per Station")

#### All Transactions by Station
count_of_all_transactions_by_station <- aggregate(count~Station,count_of_unique_by_station,sum);
names(count_of_all_transactions_by_station) = c("Station","Count")
## Sort by small to large
count_of_all_transactions_by_station$Station <- factor(count_of_all_transactions_by_station$Station, levels = count_of_all_transactions_by_station$Station[order(count_of_all_transactions_by_station$Count)])
## Plot itmode
ggplot(count_of_all_transactions_by_station, aes(x=count_of_unique_races_by_station$Station,y=Count/1000,colour=Station,fill=Station)) + geom_bar(stat = "identity") +
  ylab("Count of All Transations Per Station ('000s)") +
  xlab("Station")

## Try running some regression and plotting CART
##XXout_df$status_binary = 0
##XXout_df$status_binary[out_df$status=="success"] = 1
##XXrpart.tenMinModel = rpart(status_binary ~  convertedFiveMinCycle, data=problem_df,method="class",control=rpart.control(minbucket=25))
##XXprp(rpart.tenMinModel,extra=102,under=TRUE, varlen=0,faclen=0)

##XXrpart.fiveMinModel = rpart(status_binary ~  convertedFiveMinCycle, data=problem_df,method="class",control=rpart.control(minbucket=25))
##XXprp(rpart.fiveMinModel,extra=102,under=TRUE, varlen=0,faclen=0)

## Find lag in processor saves by station ##
out_df$convertedEDT1 <- as.POSIXct( out_df$convertedEDT, format="%y-%m-%d %H:%M:%S", tz="America/New_York") 
out_df <- out_df %>%
  arrange(convertedEDT1) %>%
  group_by(Station) %>%
  mutate(ProccessorStationLag = convertedEDT1 - lag(convertedEDT1))
## Prune the outliers (updates every 15 minutes)
out_df <- out_df[out_df$ProccessorStationLag < (15*60),]
out_df$ProccessorStationLag[is.na(out_df$ProccessorStationLag)] <- 0
out_df <- out_df[!is.na(out_df$RaceId),]
## Filter out >= 300 seconds lag as these are outliers ##
out_df <- out_df[out_df$ProccessorStationLag <= 300,]

by_station_lag <- group_by(out_df,Station)
by_station_lag <- summarize(by_station_lag, mean(ProccessorStationLag))
names(by_station_lag) <- c("Station", "AvgLag")
by_station_lag <- by_station_lag[!is.na(by_station_lag$Station),]
ggplot(by_station_lag, aes(x=Station, y=AvgLag)) + 
  geom_bar(stat="identity", aes(fill=AvgLag)) +
  xlab("Station") +
  ylab("Average Lag Time in Seconds")

## Find lag in processor saves by race id ##
out_df$convertedEDT1 <- as.POSIXct( out_df$convertedEDT, format="%y-%m-%d %H:%M:%S", tz="America/New_York") 
out_df <- out_df %>%
  arrange(convertedEDT1) %>%
  group_by(RaceId) %>%
  mutate(ProccessorRaceLag = convertedEDT1 - lag(convertedEDT1))
out_df$ProccessorRaceLag[is.na(out_df$ProccessorRaceLag)] <- 0
## Prune the outliers 5 minutes
out_df <- out_df[out_df$ProccessorRaceLag <= 300,]

by_race_lag <- group_by(out_df,RaceId)
by_race_lag <- summarize(by_race_lag, mean(ProccessorRaceLag))
names(by_race_lag) <- c("RaceId", "AvgLag")
ggplot(by_race_lag, aes(x=RaceId, y=AvgLag)) + 
  geom_bar(stat="identity", aes(fill=AvgLag)) +
  xlab("Race") +
  ylab("Average Lag Time in Seconds")


## New report to estimate when the physical file is seen and when the the actual timestamp was ##
next_file_df <- read.table(pipe("grep 'The next file is' elections.log"), sep='|',quote="\"")
next_file_df$log_time <- next_file_df$V1
next_file_df$station <- next_file_df$V1
names(next_file_df) <- c("LogTime", "FileUnixTime", "Station")
## Parse log time
next_file_df$LogTime <- gsub('].*$',"",next_file_df$LogTime)
next_file_df$LogTime <- gsub('\\[',"",next_file_df$LogTime)
tempDate <- as.POSIXct( next_file_df$LogTime, format="%m-%d %H:%M", tz="America/New_York")
next_file_df$LogTime <- tempDate
## Parse file unix time
next_file_df$FileUnixTime <- gsub('^.*elx-',"",next_file_df$FileUnixTime)
next_file_df$FileUnixTime <- gsub('^.*-',"",next_file_df$FileUnixTime)
next_file_df$FileUnixTime <- gsub('\\.zip',"",next_file_df$FileUnixTime)
tempDate1 <- as.POSIXct(as.numeric(next_file_df$FileUnixTime), origin="1970-01-01")
next_file_df$FileUnixTime <- tempDate1
## Calculate lag time
next_file_df$lagTimeInSeconds <- abs(next_file_df$FileUnixTime - next_file_df$LogTime)
## Parse station
next_file_df$Station <- gsub('^.*elx-',"",next_file_df$Station)
next_file_df$Station <- gsub('-.*$',"",next_file_df$Station)
## Remove any lag over 5 minutes as that signify regular non-new data updates
next_file_df <- next_file_df[next_file_df$lagTimeInSeconds <= 60*5,]

## Summary
by_station <- group_by(next_file_df,Station)
by_station <- summarize(by_station, mean(lagTimeInSeconds))
names(by_station) <- c("Station", "AvgLag")
## Box plot it
ggplot(data = next_file_df, aes(x=Station, y=lagTimeInSeconds)) + 
  geom_boxplot(aes(fill=Station)) +
  stat_boxplot(geom ='errorbar') + 
  xlab("Station") +
  ylab("Lag Time in Seconds") +
  labs(fill = "Station") 
  ## + facet_wrap( ~ Station, scales="free")
## Bar plot it
ggplot(by_station, aes(x=Station, y=AvgLag)) + 
  geom_bar(stat="identity", aes(fill=AvgLag)) +
  xlab("Station") +
  ylab("Average Lag Time in Seconds")

## Do time calculation from one row to next group by different station
next_file_df <- next_file_df %>%
  arrange(LogTime) %>%
  group_by(Station) %>%
  mutate(AvgLag1 = LogTime - lag(LogTime))
next_file_df$AvgLag1[is.na(next_file_df$AvgLag1)] <- 0
## Summary
by_station1 <- group_by(next_file_df,Station)
by_station1 <- summarize(by_station1, mean(AvgLag1))
names(by_station1) <- c("Station", "AvgLag")
by_station1$AvgLag1 <- as.numeric(by_station1$AvgLag)

## Box plot it
ggplot(data = next_file_df, aes(x=Station, y=AvgLag1)) + 
  geom_boxplot(aes(fill=Station)) +
  stat_boxplot(geom ='errorbar') + 
  xlab("Station") +
  ylab("Log Lag Time in Seconds") +
  labs(fill = "Station") 
## + facet_wrap( ~ Station, scales="free")

## Bar plot it
ggplot(by_station1, aes(x=Station, y=AvgLag)) + 
  geom_bar(stat="identity", aes(fill=AvgLag))  +
  xlab("Station") +
  ylab("Avg Log Lag Time in Seconds")

#overlay both bar plot, shows throughput lag overlay with processing lag
p <- ggplot(NULL, aes(Station, AvgLag)) + 
  geom_bar(aes(fill = "Avg Election Processing Lag"), colour=c("red"), data = by_station, alpha = .6,stat="identity") +
  geom_bar(aes(fill = "Avg NewsTicker Data Throughput Lag"), colour=c("blue"), data = by_station1, alpha = .7,stat="identity") +
  ylab("Average Lag In Seconds") +
  xlab("Stations")
p + guides(fill=guide_legend(title="Key"))

## Another way to plot similar line with dodge
## Join both by_station df
##by_station1 <- merge(by_station, by_station1, by.x = "Station")
by_station$type <- "Avg Election Enqueueing Lag"
by_station1$type <- "Avg NewsTicker Transfer Lag"
by_station1$AvgLag1 <- NULL

x <- rbind(by_station, by_station1)
ggplot(x, aes(x=factor(Station), y=AvgLag, fill=factor(type))) + 
  geom_bar(stat="identity", colour="black", position="dodge") + 
  ylab("Average Lag In Seconds") +
  xlab("Stations") +
  guides(fill=guide_legend(title="Key"))

## Plot enqueueing vs save lags
by_station_lag$type <- "Avg Election Save Lag"
x <- rbind(by_station, by_station_lag)
x <- x[!is.na(x$Station),]
ggplot(x, aes(x=factor(Station), y=AvgLag, fill=factor(type))) + 
  geom_bar(stat="identity", colour="black", position="dodge") + 
  ylab("Average Lag In Seconds") +
  xlab("Stations") +
  guides(fill=guide_legend(title="Key"))

## Stacking the chart
ggplot(x, aes(x=factor(Station), y=AvgLag, fill=factor(type))) + 
  geom_bar(stat="identity", colour="black") + 
  ylab("Average Lag In Seconds") +
  xlab("Stations") +
  guides(fill=guide_legend(title="Key"))

## Lag over time
## Take mean given 5 minute interval
## Processor lag
processor_lag_over_time_df = out_df %>% group_by(convertedEDTDay,five_min_bin) %>% mutate(five_min_lag = mean(ProccessorRaceLag))

next_file_df$convertedEDT <- as.POSIXct( next_file_df$LogTime, format="%y-%m-%d %H:%M:%S", tz="America/New_York") 
attr(next_file_df$convertedEDT, "tzone") <- "America/New_York"
next_file_df$convertedEDTh <- as.numeric(format(next_file_df$convertedEDT,"%H"))
next_file_df$convertedEDThm <- (next_file_df$convertedEDTh * 60) + as.numeric(format(next_file_df$convertedEDT,"%M"))
next_file_df$five_min_bin = floor((next_file_df$convertedEDThm + 1)/ 5)
next_file_df$convertedEDTDay = format(next_file_df$convertedEDT,format="%d-%B")
next_file_df$convertedEDTDayAsDate <- as.Date(next_file_df$convertedEDTDay, "%d-%B")
next_file_lag_over_time_df = next_file_df %>% group_by(convertedEDTDay,five_min_bin) %>% mutate(five_min_lag = mean(lagTimeInSeconds))

p <- ggplot(NULL, aes(x=convertedEDT,y=five_min_lag)) + 
  geom_smooth(aes(fill = "Avg Election Processing Lag"),colour=c("red"),  data = processor_lag_over_time_df, weight=1,size=1) +
  geom_smooth(aes(fill = "Avg NewsTicker Data Throughput Lag"), colour=c("blue"), data = next_file_lag_over_time_df, weight=1,size=1) +
  ylab("Average Lag In Seconds") +
  xlab("Time of Day ET")
p + guides(fill=guide_legend(title="Key"))