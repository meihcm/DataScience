library(dplyr)
require("psych")
library(psych)
require("ROCR")
library(ROCR)
require("caret")
library(caret)
library(caTools)
library(gridExtra)
projectHome <- paste("~/DataScience") ##"/Users/michaelchiem/DataScience"
datasetHome <- paste(projectHome,"/OnlineNewsPopularity",sep="")
setwd(datasetHome)
## FUNCTIONS ##
## This function uses stanford's nlp and sentiment analysis to get a mean sentiment score ##
runAnalysis <- function(model_inputs, popular_share_inputs, original_df, datasetHome) {
  ## outputs directory
  this.root_path = paste(datasetHome,"/outputs",sep="")
  ## Delete outputs
  unlink(this.root_path, recursive=TRUE)
  ## Recreate outputs dir
  dir.create(this.root_path)
  size_of_results = nrow(model_inputs)
  ## Looping all models for glm
  for (loop_index in 1:size_of_results) 
  {
    this.model_name = trimws(model_inputs$model_name[loop_index])
    this.model = trimws(model_inputs$predict_vars[loop_index])
    print(paste("Running model",this.model_name, ":", this.model,sep=""))
    size_of_thresholds = nrow(popular_share_inputs)
    
    dir.create(paste(this.root_path,"/",this.model_name,sep=""))
    setwd(paste(this.root_path,"/",this.model_name,sep=""))
    for (inner_loop_index in 1: size_of_thresholds) 
    {
      ## convert Shares to 1 or 0 for popular or not
      this.threshold_name = trimws(popular_share_inputs$share_name[inner_loop_index])
      print(paste("Running threshold",this.threshold_name,sep=""))
      
      this.threshold = trimws(popular_share_inputs$share_thresholds[inner_loop_index])
      this_df = original_df ##
      this_df$shares[this_df$shares < this.threshold] = 0
      this_df$shares[this_df$shares >= this.threshold] = 1
      this_df$shares = as.factor(this_df$shares)
      ## Split data into test and train
      ## 70% train, 30% test
      set.seed(88)
      split = sample.split(this_df$shares, SplitRatio=.7)
      this_df.training_df = subset(this_df,split==TRUE)
      this_df.testing_df = subset(this_df,split==FALSE)
      ## Begin of analysis ##
      ## Logistic model
      this.model_glm <- glm(as.character(this.model),family=binomial(link='logit'),data=this_df.training_df)
      sink(paste(this.model_name, "_", this.threshold_name, ".model_summary.txt",sep=""))
      print(paste("Using this.model:", this.model, sep=""))
      print(summary(this.model_glm))
      sink()
      ## Predict model
      this.model_predict <- predict(this.model_glm, type="response")
      this.model_prediction <- prediction(this.model_predict, this_df.training_df$shares)
      this.model_performance <- performance(this.model_prediction, measure = "tpr", x.measure = "fpr")
      plot(this.model_performance, colorize = TRUE, print.cutoffs.at=seq(0,1,.01),text.adj = c(-0.2,1.7))
      ## AUC
      this.model_auc <- performance(this.model_prediction, measure = "auc")
      this.model_auc <- this.model_auc@y.values[[1]]
      this.model_auc
      print(paste("AUC:", this.model_auc,sep=""))
    }
  }
}
## END FUNCTIONS ##
## BEGIN MAIN ##
## Read all data files ##
## Original dataset is used to find a share threshold-value 'x' so that we're confortable
## as saying anything above x is popular and below x is not popular
if(!exists("mashable_df")) {
  mashable_df <- read.table("OnlineNewsPopularity.csv", header=TRUE, sep=",", na.strings="NA")
}
stat_desc.mashable_df = describe(mashable_df$shares)

## Extra calculation to see what a normally distributed shares would look like
## First remove right skewed
stat_desc_log.mashable_df = describe(log(mashable_df$shares))## natural log
three_sd_above_normal_dist.mashable_df = exp(stat_desc_log.mashable_df$mean) + 3 * exp(stat_desc_log.mashable_df$sd)
mashable_df.normal_dist = subset(mashable_df, shares < three_sd_above_normal_dist.mashable_df)
## Next remove left skewed
stat_desc_normal_dist = describe(mashable_df.normal_dist$shares)
three_sd_below_normal_dist.mashable_df = (stat_desc_normal_dist$mean) - 3 * (stat_desc_normal_dist$sd)
mashable_df.normal_dist = subset(mashable_df.normal_dist, shares > three_sd_below_normal_dist.mashable_df)
stat_desc_normal_dist = describe(mashable_df.normal_dist$shares)
mashable_df.quantiles = summary(mashable_df$shares)

## Use the normally distributed mean
## Against original dataset
popularity_pct.normal_dist_mean = nrow(mashable_df[mashable_df$shares >= round(stat_desc_normal_dist$mean),]) / nrow(mashable_df)
popularity_pct.1st_Quantile = nrow(mashable_df[mashable_df$shares >= round(mashable_df.quantiles["1st Qu."]),]) / nrow(mashable_df)
popularity_pct.3rd_Quantile = nrow(mashable_df[mashable_df$shares >= round(mashable_df.quantiles["3rd Qu."]),]) / nrow(mashable_df)
popular_share_threshold = stat_desc_normal_dist$mean
if(!exists("new_df")) {
  new_df <- read.table("mashable_engineered.tbl", header=TRUE, sep='^', na.strings="NA")
}
## Treat NA in sentiment by using mean value
new_df$para3_sentiment[is.na(new_df$para3_sentiment)] = mean(new_df$para3_sentiment, na.rm=TRUE)
new_df$para2_sentiment[is.na(new_df$para2_sentiment)] = mean(new_df$para2_sentiment, na.rm=TRUE)
new_df$para1_sentiment[is.na(new_df$para1_sentiment)] = mean(new_df$para1_sentiment, na.rm=TRUE)
new_df$title_sentiment[is.na(new_df$title_sentiment)] = mean(new_df$title_sentiment, na.rm=TRUE)
new_df$full_sentiment[is.na(new_df$full_sentiment)] = mean(new_df$full_sentiment, na.rm=TRUE)

## Boost sentiment based on top-down user reading behavior
## The higher the more weight
## So title_sentiment = title_sentiment x 4, and para1_sentiment = para1_sentiment X 3 and so on...
new_df$title_sentiment = new_df$title_sentiment * 4
new_df$para1_sentiment = new_df$para1_sentiment * 3
new_df$para2_sentiment = new_df$para2_sentiment * 2
new_df$para3_sentiment = new_df$para3_sentiment * 1
## Boost by average of sum of the weights above
new_df$full_sentiment = new_df$full_sentiment * mean(c(4,3,2,1))

## These are models to run ##
model_name <- c("full_sentiment","def","gha")
predict_vars <- c('shares ~ full_sentiment', 
                  'shares ~ title_sentiment', 
                  'shares ~ full_sentiment')

model_inputs = data.frame(model_name, predict_vars)
model_inputs$model_name = as.character(model_inputs$model_name)
model_inputs$predict_vars = as.character(model_inputs$predict_vars)

share_name <- c('normal_mean', 'first_quantile', 'third_quantile', 'median')
share_thresholds = c(round(stat_desc_normal_dist$mean),
                     round(mashable_df.quantiles["1st Qu."]),
                     round(mashable_df.quantiles["3rd Qu."]),
                     round(stat_desc.mashable_df$median))
share_threshold_inputs = data.frame(share_name, share_thresholds)
share_threshold_inputs$share_name = as.character(share_threshold_inputs$share_name)
share_threshold_inputs$share_thresholds = as.numeric(share_threshold_inputs$share_thresholds)

##pdf("test.pdf", width=11, height=8.5)
##grid.table(run_tasks)
##grid.table(run_tasks)
##dev.off()
print(summary(new_df$shares))
## Begin of analysis ##
runAnalysis(model_inputs, share_threshold_inputs, new_df,datasetHome)
## Comparison
## Baseline number of 1s in testing / all testing rows
##baseline_testing_df = nrow(testing_df[testing_df$shares==1,]) / nrow(testing_df)
##baseline_training_df = nrow(training_df[training_df$shares==1,]) / nrow(training_df)