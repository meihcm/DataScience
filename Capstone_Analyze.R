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

## Read all data files and merge ##
mashable_df <- read.table("OnlineNewsPopularity.csv", header=TRUE, sep=",", na.strings="NA")
## Remove right skewed, as defined by shares >=100000 as only 58 of them are of that threshold
mashable_df = mashable_df[mashable_df$shares < 3000,]

new_df <- read.table("mashable_engineeredto700.tbl", header=TRUE, sep='^', na.strings="NA")
new_df1 <- read.table("mashable_engineeredto20400.tbl", header=FALSE, sep='^', na.strings="NA")
names(new_df1) = names(new_df)
new_df2 <- read.table("mashable_engineered.tbl", header=FALSE, sep='^', na.strings="NA")
names(new_df2) = names(new_df)
new_df <- rbind(new_df,new_df1,new_df2)
new_df <- new_df[new_df$shares < 3000,]

## Change fields from factor to character classes
new_df$title <- as.character(new_df$title)
new_df$para1 <- as.character(new_df$para1)
new_df$para2 <- as.character(new_df$para2)
new_df$para3 <- as.character(new_df$para3)

## Clean all new features that have empty
## Remove empty paragraph 3
new_df = new_df[trimws(new_df$para3) != "",]
new_df = new_df[!is.na(new_df$para3_sentiment),]
## Remove empty paragraph 2
new_df = new_df[trimws(new_df$para2) != "",]
new_df = new_df[!is.na(new_df$para2_sentiment),]
## Remove empty paragraph 1
new_df = new_df[trimws(new_df$para1) != "",]
new_df = new_df[!is.na(new_df$para1_sentiment),]
## Remove empty title
new_df = new_df[trimws(new_df$title) != "",]
new_df = new_df[!is.na(new_df$title_sentiment),]
## Remove full sentiment NA
new_df = new_df[!is.na(new_df$full_sentiment),]

## convert Shares to 1 or 0 for popular or not
## We are taking the mean to be the split of 1 when it is higher than mean otherwise 0
## describe(mashable_df$shares)
new_df$shares[new_df$shares < 1200] = 0
new_df$shares[new_df$shares >= 1200] = 1
new_df$shares = as.factor(new_df$shares)

## Make factor of sentiment
## 0 very negative, 1 negative, 2 neutral, 3 positive, 4 very positive
new_df$title_sentiment[new_df$title_sentiment < 1] = 0
new_df$title_sentiment[new_df$title_sentiment < 2 & new_df$title_sentiment > 0] = 1
new_df$title_sentiment[new_df$title_sentiment < 3 & new_df$title_sentiment > 1] = 2
new_df$title_sentiment[new_df$title_sentiment < 4 & new_df$title_sentiment > 2] = 3
new_df$title_sentiment[new_df$title_sentiment > 3] = 4
##new_df$title_sentiment <- as.factor(new_df$title_sentiment)
new_df$para1_sentiment[new_df$para1_sentiment < 1] = 0
new_df$para1_sentiment[new_df$para1_sentiment < 2 & new_df$para1_sentiment > 0] = 1
new_df$para1_sentiment[new_df$para1_sentiment < 3 & new_df$para1_sentiment > 1] = 2
new_df$para1_sentiment[new_df$para1_sentiment < 4 & new_df$para1_sentiment > 2] = 3
new_df$para1_sentiment[new_df$para1_sentiment > 3] = 4
##new_df$para1_sentiment <- as.factor(new_df$para1_sentiment)
new_df$para2_sentiment[new_df$para2_sentiment < 1] = 0
new_df$para2_sentiment[new_df$para2_sentiment < 2 & new_df$para2_sentiment > 0] = 1
new_df$para2_sentiment[new_df$para2_sentiment < 3 & new_df$para2_sentiment > 1] = 2
new_df$para2_sentiment[new_df$para2_sentiment < 4 & new_df$para2_sentiment > 2] = 3
new_df$para2_sentiment[new_df$para2_sentiment > 3] = 4
##new_df$para2_sentiment <- as.factor(new_df$para2_sentiment)
new_df$para3_sentiment[new_df$para3_sentiment < 1] = 0
new_df$para3_sentiment[new_df$para3_sentiment < 2 & new_df$para3_sentiment > 0] = 1
new_df$para3_sentiment[new_df$para3_sentiment < 3 & new_df$para3_sentiment > 1] = 2
new_df$para3_sentiment[new_df$para3_sentiment < 4 & new_df$para3_sentiment > 2] = 3
new_df$para3_sentiment[new_df$para3_sentiment > 3] = 4
##new_df$para3_sentiment <- as.factor(new_df$para3_sentiment)
new_df$full_sentiment[new_df$full_sentiment < 1] = 0
new_df$full_sentiment[new_df$full_sentiment < 2 & new_df$full_sentiment > 0] = 1
new_df$full_sentiment[new_df$full_sentiment < 3 & new_df$full_sentiment > 1] = 2
new_df$full_sentiment[new_df$full_sentiment < 4 & new_df$full_sentiment > 2] = 3
new_df$full_sentiment[new_df$full_sentiment > 3] = 4
##new_df$full_sentiment <- as.factor(new_df$full_sentiment)

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
model2 <- glm(shares ~ title_sentiment
                ,family=binomial(link='logit'),data=training_df)
summary(model2)

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
table(training_df$shares, p_model2 > 0.22)
pr_model2 <- prediction(p_model2, training_df$shares)
prf_model2 <- performance(pr_model2, "tpr", "fpr")
plot(prf_model2, colorize = TRUE, print.cutoffs.at=seq(0,1,.01),text.adj = c(-0.2,1.7))
## AUC
auc_model2 <- performance(pr_model2, measure = "auc")
auc_model2 <- auc_model2@y.values[[1]]
auc_model2

## Comparison
## Baseline number of 1s in testing / all testing rows
baseline_testing_df = nrow(testing_df[testing_df$shares==1,]) / nrow(testing_df)
baseline_training_df = nrow(training_df[training_df$shares==1,]) / nrow(training_df)