library(dplyr)
install.packages("psych")
library(psych)
install.packages("ROCR")
library(ROCR)
install.packages("caret")
library(caret)
library(caTools)
projectHome <- paste("~/DataScience") ##"/Users/michaelchiem/DataScience"
datasetHome <- paste(projectHome,"/OnlineNewsPopularity",sep="")
setwd(datasetHome)

## Read all data files ##
## Original dataset is used to find a share threshold-value 'x' so that we're confortable
## as saying anything above x is popular and below x is not popular
mashable_df <- read.table("OnlineNewsPopularity.csv", header=TRUE, sep=",", na.strings="NA")
describe(mashable_df$shares)
## Original popularity if based on mean
## Anything greater than mean would have only been about 20%
original_popularity_pct = nrow(mashable_df[mashable_df$shares >= 3395,]) / nrow(mashable_df)
## Assign it to 1400 (the median) as it represents close to a 50/50 split
## Motivation is that we want a relax way to detect for popularity
relaxed_popularity_pct = nrow(mashable_df[mashable_df$shares >= 1401,]) / nrow(mashable_df)
popular_share_threshold = 1401

new_df <- read.table("mashable_engineered.tbl", header=TRUE, sep='^', na.strings="NA")

## Change fields from factor to character classes
new_df$title <- as.character(new_df$title)
new_df$para1 <- as.character(new_df$para1)
new_df$para2 <- as.character(new_df$para2)
new_df$para3 <- as.character(new_df$para3)

## Treat NA in sentiment by using mean value
summary(new_df$para3_sentiment) ## Using mean of 1.378
new_df$para3_sentiment[is.na(new_df$para3_sentiment)] = 1.378
summary(new_df$para3_sentiment) ## Using mean of 1.347
new_df$para2_sentiment[is.na(new_df$para2_sentiment)] = 1.347
summary(new_df$para3_sentiment) ## Using mean of 1.395
new_df$para1_sentiment[is.na(new_df$para1_sentiment)] = 1.395
summary(new_df$title_sentiment) ## Using mean of 1.453
new_df$title_sentiment[is.na(new_df$title_sentiment)] = 1.453
summary(new_df$full_sentiment) ## Using mean of 1.462
new_df$full_sentiment[is.na(new_df$full_sentiment)] = 1.462

## convert Shares to 1 or 0 for popular or not
## We are taking the mean to be the split of 1 when it is higher than mean otherwise 0
## describe(mashable_df$shares)
nrow(new_df[new_df$shares > popular_share_threshold,])## 1401 seems to split 1 and 0 into half
new_df$shares[new_df$shares < popular_share_threshold] = 0
new_df$shares[new_df$shares >= popular_share_threshold] = 1
new_df$shares = as.factor(new_df$shares)

## Split data into test and train
## 60% train, 40% test
set.seed(88)
split = sample.split(new_df$shares, SplitRatio=.6)
training_df = subset(new_df,split==TRUE)
testing_df = subset(new_df,split==FALSE)

## Logistic model
model1 <- glm(shares ~ title_sentiment + para1_sentiment + para2_sentiment 
              + para3_sentiment + full_sentiment,family=binomial(link='logit'),data=training_df)
summary(model1)
model2 <- glm(shares ~ title_sentiment + para1_sentiment
                ,family=binomial(link='logit'),data=training_df)
summary(model2)
## Add article category/channels and image features, 
## found that title_sentiment is not significant
model3 <- glm(shares ~ weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday 
               + weekday_is_thursday + weekday_is_friday + weekday_is_saturday 
               + data_channel_is_entertainment + data_channel_is_bus 
               + data_channel_is_socmed + data_channel_is_world 
               + num_imgs
              + para1_sentiment,
               family=binomial(link='logit'),data=training_df)
summary(model3)
## Look at the coefficients confidence interval
round(exp(cbind(Estimate=coef(model3),confint(model3))),2)

## Predict model 1
p_model1 <- predict(model1, type="response")
pr_model1 <- prediction(p_model1, training_df$shares)
prf_model1 <- performance(pr_model1, measure = "tpr", x.measure = "fpr")
plot(prf_model1, colorize = TRUE, print.cutoffs.at=seq(0,1,.01),text.adj = c(-0.2,1.7))
## AUC
auc_model1 <- performance(pr_model1, measure = "auc")
auc_model1 <- auc_model1@y.values[[1]]
auc_model1

## Predict model 2
p_model2 <- predict(model2, type="response")
## Small threshold will allow for larger 1 errors
summary(p_model2)
pr_model2 <- prediction(p_model2, training_df$shares)
prf_model2 <- performance(pr_model2, "tpr", "fpr")
plot(prf_model2, colorize = TRUE, print.cutoffs.at=seq(0,1,.01),text.adj = c(-0.2,1.7))
## AUC
auc_model2 <- performance(pr_model2, measure = "auc")
auc_model2 <- auc_model2@y.values[[1]]
auc_model2

## Predict model 3
p_model3 <- predict(model3, type="response")
## Small threshold will allow for larger 1 errors
summary(p_model3)
pr_model3 <- prediction(p_model3, training_df$shares)
prf_model3 <- performance(pr_model3, "tpr", "fpr")
plot(prf_model3, colorize = TRUE, print.cutoffs.at=seq(0,1,.1),text.adj = c(-0.3,1.7))
## AUC
auc_model3 <- performance(pr_model3, measure = "auc")
auc_model3 <- auc_model3@y.values[[1]]
auc_model3

## Comparison
## Baseline number of 1s in testing / all testing rows
baseline_testing_df = nrow(testing_df[testing_df$shares==1,]) / nrow(testing_df)
baseline_training_df = nrow(training_df[training_df$shares==1,]) / nrow(training_df)