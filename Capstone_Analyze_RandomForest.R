library(randomForest)
forest_train = randomForest(shares ~ 
                              LDA_00 + LDA_01 + LDA_02 + LDA_03 + global_subjectivity, data = training_df)
print(forest_train)
testforest = predict(forest_train, newdata=testing_df)
table(testforest, testing_df$shares) #confusion matrix for test set



> #prepare model for ROC Curve
test.forest = predict(forest_train, type = "prob")
forestpred = prediction(test.forest[,2], training_df$shares)
forestperf = performance(forestpred, "tpr", "fpr")
forestperfauc = performance(forestpred, measure="auc")
forestperfauc@y.values[[1]]
plot(forestperf, col=1, add=TRUE)
legend(0.6, 0.6, c('rforest'), 1:3)