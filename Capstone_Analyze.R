if("dplyr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("dplyr")
}
library(dplyr)
if("psych" %in% rownames(installed.packages()) == FALSE) {
  install.packages("psych")
}
library(psych)
if("ROCR" %in% rownames(installed.packages()) == FALSE) {
  install.packages("ROCR")
}
library(ROCR)
if("caret" %in% rownames(installed.packages()) == FALSE) {
  install.packages("caret")
}
library(caret)
if("caTools" %in% rownames(installed.packages()) == FALSE) {
  install.packages("caTools")
}
library(caTools)
if("gridExtra" %in% rownames(installed.packages()) == FALSE) {
  install.packages("caTools")
}
library(gridExtra)
## Clear out all vars
##rm(list = ls())

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
  size_of_thresholds = nrow(popular_share_inputs)
  compare_analysis_table <- data.frame(model=rep("", size_of_thresholds), 
                                       glm_model=rep("", size_of_thresholds),
                                       threshold=rep("", size_of_thresholds),
                                       threshold_value=rep("", size_of_thresholds),
                                       actual_popular_count=rep("", size_of_thresholds),
                                       population_size=rep("", size_of_thresholds),
                                       aic=rep("", size_of_thresholds),
                                       auc=rep("", size_of_thresholds),
                                       recall=rep("", size_of_thresholds),
                                       precision=rep("", size_of_thresholds),
                                       fscore=rep("", size_of_thresholds),
                                       accuracy=rep("", size_of_thresholds),
                                       stringsAsFactors=FALSE)
  iteration_counter = 0
  ## Looping all models for glm
  for (loop_index in 1:size_of_results) 
  {
    this.model_name = trimws(model_inputs$model_name[loop_index])
    this.model = trimws(model_inputs$predict_vars[loop_index])
    print(paste("Running model:",this.model_name, "-", this.model,sep=""))
    
    dir.create(paste(this.root_path,"/",this.model_name,sep=""))
    setwd(paste(this.root_path,"/",this.model_name,sep=""))
    for (inner_loop_index in 1: size_of_thresholds) 
    {
      iteration_counter = iteration_counter + 1
      ## convert Shares to 1 or 0 for popular or not
      this.threshold_name = trimws(popular_share_inputs$share_name[inner_loop_index])
      print(paste("Running threshold: ",this.threshold_name,sep=""))
      
      this.threshold = trimws(popular_share_inputs$share_thresholds[inner_loop_index])
      this_df = original_df ##
      this_df$shares[this_df$shares < strtoi(this.threshold)] = 0
      this_df$shares[this_df$shares >= strtoi(this.threshold)] = 1
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
      this.model_aic <- AIC(this.model_glm)
      ## Predict model
      this.model_predict <- predict(this.model_glm, type="response")
      this.model_prediction <- prediction(this.model_predict, this_df.training_df$shares)
      this.model_performance <- performance(this.model_prediction, measure = "tpr", x.measure = "fpr")
      ## Confusion table      
      this.predicted_training_popularity = rep(1, nrow(this_df.training_df))
      this.predicted_training_popularity[this.model_predict <=.50] = 0
      ## Add only one 0 so that confusion matrix is not unbalanced
      if(sum(this.predicted_training_popularity)/nrow(this_df.training_df) == 1)
      {
        this.predicted_training_popularity[1] = 0
      }  
      ## Prediction are rows, truth are columns
      this.predicted_training_confusion_table = table(this.predicted_training_popularity,this_df.training_df$shares)
      
      ## Calculate Recall, Precision, F1 Score
      true_negative = this.predicted_training_confusion_table[1,1]
      false_negative = this.predicted_training_confusion_table[1,2]
      true_positive = 0
      false_positive = 0
      if(nrow(this.predicted_training_confusion_table) > 1) {
        false_positive = this.predicted_training_confusion_table[2,1]
        true_positive = this.predicted_training_confusion_table[2,2]
      }
      ## tp / (tp + fn)
      this.predicted_training_recall = (true_positive/(true_positive + false_negative))
      ## tp / (tp + fp)
      this.predicted_training_precision = (true_positive/(true_positive + false_positive))
      this.predicted_training_f1score = 2 * this.predicted_training_precision * this.predicted_training_recall/ (this.predicted_training_recall + this.predicted_training_precision)
      ## (tp+tn)/(tp+tn+fp+fn)
      this.predicted_training_accuracy = (true_positive + true_negative)/(true_positive + true_negative + false_positive + false_negative)
        
      pdf(paste(this.model_name, "_", this.threshold_name, ".model_roc.pdf",sep=""), width=11, height=8.5)
      plot(this.model_performance, colorize = TRUE, print.cutoffs.at=seq(0,1,.1),text.adj = c(-0.2,1.7))
      dev.off()   
      ## AUC
      this.model_auc <- performance(this.model_prediction, measure = "auc")
      this.model_auc <- this.model_auc@y.values[[1]]
      this.model_auc
      
      ## Output results
      sink(paste(this.model_name, "_", this.threshold_name, ".model_summary.txt",sep=""))
      print(paste("Threshold: ", this.threshold_name, " (", this.threshold,") - number of popular articles: ",nrow(this_df.training_df[this_df.training_df$shares == 1,]), ", number of total articles: ", nrow(this_df.training_df),sep=""))
      print("=============================================================================================")
      print(paste("Using this.model:", this.model, sep=""))
      print(summary(this.model_glm))
      print("=============================================================================================")
      print(paste("AUC:", this.model_auc,sep=""))
      print("")
      print("CONFUSION TABLE: (Prediction are rows, Truth are columns)")
      print(this.predicted_training_confusion_table)
      print("")
      print(paste("Recall (Correctly Predicted Popular / Actual Popular): ", this.predicted_training_recall), sep="")
      print(paste("Precision (Correctly Predicted Popular / All Predicted Popular): ", this.predicted_training_precision), sep="")
      print(paste("F1-Score: ", this.predicted_training_f1score), sep="")
      sink()
      ## Save the result in a summary data frame
      compare_analysis_table[iteration_counter,1] = this.model_name
      compare_analysis_table[iteration_counter,2] = this.model
      compare_analysis_table[iteration_counter,3] = this.threshold_name
      compare_analysis_table[iteration_counter,4] = this.threshold
      compare_analysis_table[iteration_counter,5] = nrow(this_df.training_df[this_df.training_df$shares == 1,])
      compare_analysis_table[iteration_counter,6] = nrow(this_df.training_df)
      compare_analysis_table[iteration_counter,7] = this.model_aic
      compare_analysis_table[iteration_counter,8] = this.model_auc
      compare_analysis_table[iteration_counter,9] = this.predicted_training_recall
      compare_analysis_table[iteration_counter,10] = this.predicted_training_precision
      compare_analysis_table[iteration_counter,11] = this.predicted_training_f1score
      compare_analysis_table[iteration_counter,12] = this.predicted_training_accuracy
    }
  }
  ## Make certain feature numerics
  compare_analysis_table$threshold_value=as.numeric(compare_analysis_table$threshold_value)
  compare_analysis_table$aic=as.numeric(compare_analysis_table$aic)
  compare_analysis_table$auc=as.numeric(compare_analysis_table$auc)
  compare_analysis_table$recall=as.numeric(compare_analysis_table$recall)
  compare_analysis_table$precision=as.numeric(compare_analysis_table$precision)
  compare_analysis_table$fscore=as.numeric(compare_analysis_table$fscore)
  compare_analysis_table$actual_popular_count = as.numeric(compare_analysis_table$actual_popular_count)
  compare_analysis_table$population_size = as.numeric(compare_analysis_table$population_size)
  compare_analysis_table$accuracy = as.numeric(compare_analysis_table$accuracy)
  
  setwd(this.root_path)
  sink("performance_summary.txt")
  print(compare_analysis_table)
  sink()
  
  return (compare_analysis_table)
}
## END FUNCTIONS ##
## BEGIN MAIN ##
## Read all data files ##
## Original dataset is used to find a share threshold-value 'x' so that we're confortable
## as saying anything above x is popular and below x is not popular
if(!exists("mashable_df")) {
  mashable_df <- read.table("OnlineNewsPopularity.csv", header=TRUE, sep=",", na.strings="NA")
  ## Subset to features and response we want
  mashable_df = subset(mashable_df, select=c(shares,num_imgs,num_videos,data_channel_is_lifestyle,data_channel_is_entertainment,data_channel_is_bus,data_channel_is_socmed,data_channel_is_tech,data_channel_is_world,weekday_is_monday,weekday_is_tuesday,weekday_is_wednesday,weekday_is_thursday,weekday_is_friday,weekday_is_saturday,weekday_is_sunday,is_weekend))
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
  ## Subset to features and response we want
  new_df = subset(new_df, select=c(shares,num_imgs,num_videos,data_channel_is_lifestyle,data_channel_is_entertainment,data_channel_is_bus,data_channel_is_socmed,data_channel_is_tech,data_channel_is_world,weekday_is_monday,weekday_is_tuesday,weekday_is_wednesday,weekday_is_thursday,weekday_is_friday,weekday_is_saturday,weekday_is_sunday,is_weekend,title_sentiment,para1_sentiment,para2_sentiment,para3_sentiment,full_sentiment))
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
  
  ## Add columns for logs, add a minor decimal value to allow log to work
  new_df$num_imgs.log = log(new_df$num_imgs + .0001)
  new_df$num_videos.log = log(new_df$num_videos + .0001)
  new_df$data_channel_is_lifestyle.log = log(new_df$data_channel_is_lifestyle + .0001)
  new_df$data_channel_is_entertainment.log = log(new_df$data_channel_is_entertainment + .0001)
  new_df$data_channel_is_bus.log = log(new_df$data_channel_is_bus + .0001)
  new_df$data_channel_is_socmed.log = log(new_df$data_channel_is_socmed + .0001)
  new_df$data_channel_is_tech.log = log(new_df$data_channel_is_tech + .0001)
  new_df$data_channel_is_world.log = log(new_df$data_channel_is_world + .0001)
  new_df$weekday_is_monday.log = log(new_df$weekday_is_monday + .0001)
  new_df$weekday_is_tuesday.log = log(new_df$weekday_is_tuesday + .0001)
  new_df$weekday_is_wednesday.log = log(new_df$weekday_is_wednesday + .0001)
  new_df$weekday_is_thursday.log = log(new_df$weekday_is_thursday + .0001)
  new_df$weekday_is_friday.log = log(new_df$weekday_is_friday + .0001)
  new_df$weekday_is_saturday.log = log(new_df$weekday_is_saturday + .0001)
  new_df$weekday_is_sunday.log = log(new_df$weekday_is_sunday + .0001)
  new_df$is_weekend.log = log(new_df$is_weekend + .0001)
  new_df$title_sentiment.log = log(new_df$title_sentiment + .0001)
  new_df$para1_sentiment.log = log(new_df$para1_sentiment + .0001)
  new_df$para2_sentiment.log = log(new_df$para2_sentiment + .0001)
  new_df$para3_sentiment.log = log(new_df$para3_sentiment + .0001)
  new_df$full_sentiment.log = log(new_df$full_sentiment + .0001)
}

## These are models to run ##
model_name <- c("full_model","full_model_optimized","full_model_log", "full_model_log_optimized")
predict_vars <- c('shares ~ num_imgs + num_videos + data_channel_is_lifestyle + data_channel_is_entertainment + data_channel_is_bus + data_channel_is_socmed + data_channel_is_tech + data_channel_is_world + weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + weekday_is_friday + weekday_is_saturday + weekday_is_sunday + is_weekend + title_sentiment + para1_sentiment + para2_sentiment + para3_sentiment + full_sentiment'
                  ,'shares ~ num_imgs + num_videos + data_channel_is_lifestyle + data_channel_is_entertainment + data_channel_is_bus + data_channel_is_socmed + data_channel_is_tech + data_channel_is_world + weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + weekday_is_friday + para3_sentiment + full_sentiment'
                  ,'shares ~ num_imgs.log + num_videos.log + data_channel_is_lifestyle.log + data_channel_is_entertainment.log + data_channel_is_bus.log + data_channel_is_socmed.log + data_channel_is_tech.log + data_channel_is_world.log + weekday_is_monday.log + weekday_is_tuesday.log + weekday_is_wednesday.log + weekday_is_thursday.log + weekday_is_friday.log + weekday_is_saturday.log + weekday_is_sunday.log + is_weekend.log + title_sentiment.log + para1_sentiment.log + para2_sentiment.log + para3_sentiment.log + full_sentiment.log' 
                  ,'shares ~ num_imgs.log + num_videos.log + data_channel_is_lifestyle.log + data_channel_is_entertainment.log + data_channel_is_bus.log + data_channel_is_socmed.log + data_channel_is_tech.log + data_channel_is_world.log + weekday_is_monday.log + weekday_is_tuesday.log + weekday_is_wednesday.log + weekday_is_thursday.log + weekday_is_friday.log + full_sentiment.log'
                  )

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

print(summary(new_df$shares))
## Begin of analysis ##
summary_df = runAnalysis(model_inputs, share_threshold_inputs, new_df,datasetHome)