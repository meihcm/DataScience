library(randomForest)
forest_train = randomForest(shares ~ 
                              num_imgs + num_videos + data_channel_is_lifestyle + data_channel_is_entertainment 
                            + data_channel_is_bus + data_channel_is_socmed + data_channel_is_tech 
                            + data_channel_is_world + weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday 
                            + weekday_is_thursday + weekday_is_friday , data = training_df)
print(forest_train)
testforest = predict(forest_train, newdata=testing_df)
table(testforest, testing_df$shares) #confusion matrix for test set



> #prepare model for ROC Curve
test.forest = predict(forest_train, type = "prob", newdata = testing_df)
forestpred = prediction(test.forest[,2], testing_df$shares)
forestperf = performance(forestpred, "tpr", "fpr")
plot(forestperf, col=1, add=TRUE)
legend(0.6, 0.6, c('rforest'), 1:3)