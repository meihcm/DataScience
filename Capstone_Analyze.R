library(dplyr)
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
## Mean is 1300, median is 1200, since we want to relax definition of popularity, we will use median
## Everything equal/above 1200 is 1, everything below is 0
## describe(mashable_df$shares)
new_df$shares[new_df$shares < 1921] = 0
new_df$shares[new_df$shares >= 1921] = 1
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
## remove paragraph1
model2 <- glm(shares ~ title_sentiment + para2_sentiment + para3_sentiment + full_sentiment,family=binomial(link='logit'),data=training_df)
## remove full_sentiment
model3 <- glm(shares ~ title_sentiment + para2_sentiment + para3_sentiment,family=binomial(link='logit'),data=training_df)
## remove paragraph3
model4 <- glm(shares ~ title_sentiment + para2_sentiment,family=binomial(link='logit'),data=training_df)
## remove paragraph2
model5 <- glm(shares ~ title_sentiment,family=binomial(link='logit'),data=training_df)

model11 <- glm(shares ~ title_sentiment + para2_sentiment 
               + data_channel_is_lifestyle + data_channel_is_entertainment 
               + data_channel_is_bus + data_channel_is_socmed + data_channel_is_tech 
               + data_channel_is_world,family=binomial(link='logit'),data=training_df)
model12 <-glm(shares ~ 
                num_imgs + num_videos + data_channel_is_lifestyle + data_channel_is_entertainment 
              + data_channel_is_bus + data_channel_is_socmed + data_channel_is_tech 
              + data_channel_is_world + weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday 
              + weekday_is_thursday + weekday_is_friday 
                ,family=binomial(link="logit"), data=training_df)
summary(model4)

## Predict
p <- predict(model4, newdata=testing_df, type="response")
pr <- prediction(p, testing_df$shares)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
table(pr$labels,testing_df$shares)


## Find baseline model's ability to predict popular
confusionMatrix(p,testing_df$shares)

baseline = nrow(new_df[new_df$shares == 1,])/nrow(new_df)



par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
# plotting the ROC curve
plot(prf,col="black",lty=3, lwd=3)
# calculating AUC
auc <- performance(pr,"auc")
# now converting S4 class to vector
auc <- unlist(slot(auc, "y.values"))
# adding min and max ROC AUC to the center of the plot
minauc<-min(round(auc, digits = 2))
maxauc<-max(round(auc, digits = 2))
minauct <- paste(c("min(AUC)  = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
legend(0.3,0.6,c(minauct,maxauct,"\n"),border="white",cex=1.7,box.col = "white")
#
