library(randomForest)
forest_train = randomForest(shares ~ 
                              LDA_00 + LDA_01 + LDA_02 + LDA_03 + global_subjectivity, data = training_df)
##forest_train = randomForest(shares ~ title_sentiment + para1_sentiment, data = training_df)
forest_train = randomForest(shares ~ weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday 
                            + weekday_is_thursday + weekday_is_friday + weekday_is_saturday 
                            + data_channel_is_entertainment + data_channel_is_bus 
                            + data_channel_is_socmed + data_channel_is_world 
                            + num_imgs
                            + para1_sentiment, data = training_df)
print(forest_train)
testforest = predict(forest_train, newdata=testing_df)
table(testforest, testing_df$shares) #confusion matrix for test set

#prepare model for ROC Curve
test.forest = predict(forest_train, type = "prob", newdata = testing_df)
forestpred = prediction(test.forest[,2], testing_df$shares)
forestperf = performance(forestpred, "tpr", "fpr")
forestperfauc = performance(forestpred, measure="auc")
forestperfauc@y.values[[1]]
plot(forestperf, col=1, add=TRUE)
legend(0.6, 0.6, c('rforest'), 1:3)