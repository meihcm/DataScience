install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
library(ROCR)
## Assumes that the Capstone_Analyze.R file has run as it will build the same split of train
## and test dataset
rpart.model1 = rpart(shares ~  data_channel_is_entertainment + data_channel_is_bus 
              + data_channel_is_socmed + data_channel_is_world 
              ,data=training_df,method="class",
              control=rpart.control(minbucket=25)
              )
## Shows the predicted of each node and total of each node
prp(rpart.model1,extra=102,under=TRUE, varlen=0,faclen=0)
rpart.predict = predict(rpart.model1, newdata=testing_df, type="class")
## Confusion Matrix
table(testing_df$shares,rpart.predict)
rpart.model1.accuracy = (5240 + 4399) / (5240 + 4399 + 2772 + 3407)
## ROC Curve
rpart.model1.ROC = predict(rpart.model1,newdata=testing_df)
rpart.model1.ROCPrediction = prediction(rpart.model1.ROC[,2], testing_df$shares)
rpart.model1.ROCperformance = performance(rpart.model1.ROCPrediction, "tpr", "fpr")
plot(rpart.model1.ROCperformance)
## AUC
rpart.model1.AUC <- performance(rpart.model1.ROCPrediction, measure = "auc")
rpart.model1.AUC <- rpart.model1.AUC@y.values[[1]]
rpart.model1.AUC