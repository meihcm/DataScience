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
setwd("/Users/michaelchiem/DataScience")
############################################## Doing full parse from raw file
## Parse errors
errors <- read.table(pipe("grep 'Failed Save' nbc_elections.log"), sep='|',quote="\"")
errors$Time <- errors$V1
errors$Thread <-errors$V1
errors$RaceId <-errors$V1
errors$Station <- errors$V1
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

## Parse successes
successes <- read.table(pipe("grep 'Success Save' nbc_elections.log"), sep='|', quote="\"")
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
ggplot(top_ten_errant_race, aes(x=RaceId,y=all_time_race_failure_rate_average,colour=RaceId,fill=RaceId)) + 
  geom_bar(stat = "identity",width=0.4) + coord_flip() + xlab("Top Race Id") + ylab("Average Error")

## Box plotting top_ten across the 4 days
box_top_ten_errant_race = out_df[out_df$RaceId %in% top_ten_errant_race$RaceId,]
box_top_ten_errant_race = aggregate(box_top_ten_errant_race,by=list(box_top_ten_errant_race$RaceId,box_top_ten_errant_race$five_min_bin_failure_rate, box_top_ten_errant_race$convertedEDTDay),FUN=length)
names(box_top_ten_errant_race) = c("RaceId", "five_min_bin_failure_rate","convertedEDTDay")
box_top_ten_errant_race = box_top_ten_errant_race[,1:3]
ggplot(data = box_top_ten_errant_race, aes(x=convertedEDTDay, y=five_min_bin_failure_rate)) + 
  geom_boxplot(aes(fill=convertedEDTDay)) +
  stat_boxplot(geom ='errorbar') + 
  xlab("Day") +
  ylab("Failure Rate Observations (5 Min Intervals)") +
  labs(fill = "Day") +
  facet_wrap( ~ RaceId, scales="free")

## Box plotting all races across the 4 days
box_all_errant_race = out_df
box_all_errant_race = aggregate(box_all_errant_race,by=list(box_all_errant_race$RaceId,box_all_errant_race$five_min_bin_failure_rate, box_all_errant_race$convertedEDTDay),FUN=length)
names(box_all_errant_race) = c("RaceId", "five_min_bin_failure_rate","convertedEDTDay")
box_all_errant_race = box_all_errant_race[,1:3]
ggplot(data = box_all_errant_race, aes(x=convertedEDTDay, y=five_min_bin_failure_rate)) + 
  geom_boxplot(aes(fill=convertedEDTDay)) +
  stat_boxplot(geom ='errorbar') + 
  xlab("Day") +
  ylab("Failure Rate Observations (5 Min Intervals)") +
  labs(fill = "Day") +
  facet_wrap( ~ convertedEDTDay, scales="free")

## Plotting top_ten on June 10th by hours
box_top_ten_errant_race_june_10 = out_df[out_df$RaceId %in% top_ten_errant_race$RaceId & out_df$convertedEDTDay=="10-June",]
box_top_ten_errant_race_june_10 = aggregate(box_top_ten_errant_race_june_10,by=list(box_top_ten_errant_race_june_10$RaceId,box_top_ten_errant_race_june_10$convertedEDTh, box_top_ten_errant_race_june_10$convertedEDThm,box_top_ten_errant_race_june_10$five_min_bin_failure_rate),FUN=length)
names(box_top_ten_errant_race_june_10) = c("RaceId", "convertedEDTh","convertedEDThm","five_min_bin_failure_rate")
box_top_ten_errant_race_june_10 = box_top_ten_errant_race_june_10[,1:4]
box_top_ten_errant_race_june_10 = aggregate(box_top_ten_errant_race_june_10,by=list(box_top_ten_errant_race_june_10$RaceId,box_top_ten_errant_race_june_10$convertedEDTh),FUN=mean)
box_top_ten_errant_race_june_10 = box_top_ten_errant_race_june_10[,c(1,2,6)]
names(box_top_ten_errant_race_june_10) = c("RaceId", "convertedEDTh","avg_five_min_bin_failure_rate")
ggplot(data = box_top_ten_errant_race_june_10, aes(x=convertedEDTh, y=avg_five_min_bin_failure_rate)) + 
  geom_line(aes()) +
  xlab("Hour") +
  ylab("Average Failure Rate") +
  facet_wrap( ~ RaceId, scales="free")


## General plots
## Full errors
ggplot(data=subset(out_df, status=="fail"),mapping=aes(x=(convertedEDThm/60))) + 
  geom_histogram(bins=288, color="red") + 
  facet_wrap(~convertedEDTDay) + 
  xlab("Time of Day (EDT)") + 
  geom_hline(yintercept=500,linetype="dashed",color="red")

## Full successes
ggplot(data=subset(out_df, status=="success"),mapping=aes(x=(convertedEDThm/60))) + geom_histogram(bins=288, color="cyan3") + 
  facet_wrap(~convertedEDTDay) + 
  xlab("Time of Day (EDT)") + 
  geom_hline(yintercept=500,linetype="dashed",color="red") 

## Combined success and failure on one graph
ggplot(out_df, aes(convertedEDThm/60)) +
  geom_freqpoly(aes(group = status, colour = status), bins=288) + facet_wrap(~convertedEDTDay) +
  xlab("Time of Day (EDT)") + 
  geom_hline(yintercept=500,linetype="dashed",color="red") 

## Combined failure rate
grouped_failure_rate = aggregate(out_df,by=list(out_df$five_min_bin,out_df$five_min_bin_failure_rate,out_df$convertedEDTDay),FUN=length)
names(grouped_failure_rate) = c("five_min_bin", "five_min_bin_failure_rate","convertedEDTDay","count")
## Plot it
LtoM <-colorRampPalette(c('red', 'yellow' ))
Mid <- "snow3"
MtoH <-colorRampPalette(c('orange', 'red'))
ggplot(grouped_failure_rate, aes(x=five_min_bin * 5/60,y=five_min_bin_failure_rate,fill=five_min_bin_failure_rate)) +
  geom_bar(stat="identity") + facet_wrap(~convertedEDTDay) +
  xlab("Time of Day (EDT)") +
  ylab("Error Rate (Interval Error / Full Day Transaction)") +
  labs(fill="Error Rate") +
  scale_fill_gradient2(low=LtoM(100), mid='snow3', 
                       high=MtoH(100), space='Lab')

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
##  geom_freqpoly(aes(group = status, colour = status), bins=144) +
##  xlab("Time of Day (EDT)") + 
##  geom_hline(yintercept=500,linetype="dashed",color="red") 

ggplot(out_df, aes(convertedEDT)) +
  geom_freqpoly(aes(group = status, colour = status), bins=numOfFiveBins) +
  xlab("Time of Day (EDT)") + 
  geom_hline(yintercept=500,linetype="dashed",color="red") 

## See if the 10 minute interval cycle visual has any hint of when the api resets from the problem_df dataset
## See if the x axis is ten minute cycle has more clues
ggplot(problem_df, aes(convertedTenMinCycle)) +
  geom_bar(aes(group = status, colour = status, fill=status)) +
  xlab("10 Minute Cycles") + 
  facet_wrap(~Station) +
  geom_hline(yintercept=500,linetype="dashed",color="red") 

## Now expand it to all data
ggplot(out_df, aes(convertedTenMinCycle)) +
  geom_bar(aes(group = status, colour = status, fill=status)) +
  xlab("10 Minute Cycles") + 
  facet_wrap(~Station) +
  geom_hline(yintercept=500,linetype="dashed",color="red") 

## Only failures of problem_df
problem_fail_only_df <- subset(problem_df, status=="fail")
ten_min_interval_problem_df <- aggregate(problem_fail_only_df, by=list(problem_fail_only_df$Station, problem_fail_only_df$convertedTenMinCycle), FUN=length)
names(ten_min_interval_problem_df) = c("Station","convertedTenMinCycle","Count")
ten_min_interval_problem_df <- ten_min_interval_problem_df[,1:3]
## Now plot it
ggplot(ten_min_interval_problem_df,aes(x=convertedTenMinCycle,y=Count)) + geom_bar(stat="identity") + facet_wrap(~Station)

## Now expand it to across all data with failures
only_failed_df = subset(out_df, status=="fail")
ten_min_interval_all_df <- aggregate(only_failed_df, by=list(only_failed_df$Station, only_failed_df$convertedTenMinCycle), FUN=length)
names(ten_min_interval_all_df) = c("Station","convertedTenMinCycle","Count")
ten_min_interval_all_df <- ten_min_interval_all_df[,1:3]
## Now plot it
ggplot(ten_min_interval_all_df,aes(x=convertedTenMinCycle,y=Count)) + geom_bar(stat="identity") + facet_wrap(~Station)

## How bout map successes and not failures
only_success_df = subset(out_df, status=="success")
ten_min_interval_all_df <- aggregate(only_success_df, by=list(only_success_df$Station, only_success_df$convertedTenMinCycle), FUN=length)
names(ten_min_interval_all_df) = c("Station","convertedTenMinCycle","Count")
ten_min_interval_all_df <- ten_min_interval_all_df[,1:3]
## Now plot it
ggplot(ten_min_interval_all_df,aes(x=convertedTenMinCycle,y=Count)) + geom_bar(stat="identity") + facet_wrap(~Station)

## Over time success and failures by station (all station/all day)
ggplot(out_df, aes(convertedEDT)) +
  geom_freqpoly(aes(group = status, colour = status), bins=numOfFiveBins) + facet_wrap(~Station) +
  xlab("Time of Day (EDT)") + 
  geom_hline(yintercept=500,linetype="dashed",color="red") 

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
out_df$status_binary = 0
out_df$status_binary[out_df$status=="success"] = 1
rpart.tenMinModel = rpart(status_binary ~  convertedTenMinCycle, data=problem_df,method="class",control=rpart.control(minbucket=25))
prp(rpart.tenMinModel,extra=102,under=TRUE, varlen=0,faclen=0)

rpart.fiveMinModel = rpart(status_binary ~  convertedFiveMinCycle, data=problem_df,method="class",control=rpart.control(minbucket=25))
prp(rpart.fiveMinModel,extra=102,under=TRUE, varlen=0,faclen=0)