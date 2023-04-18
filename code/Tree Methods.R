### ST4248 Term Paper ###
### Tree-Based Methods ###

### Random Forest ###
library(randomForest)
set.seed(1)
rf.obesity = randomForest(obesity ~., data = final_df, subset = train, mtry = 4, importance = TRUE)
importance(rf.obesity)
varImpPlot(rf.obesity)

train_df = final_df[train,]

# tuning m
set.seed(1)
k = 10
fold.error = rep(0, k)
n = length(train_df$obesity)
index = sample(n)
foldbreaks = c(0, floor(n/k * 1:k))
cv.acc.k = c()

for(i in 1:13) {
  for(fold in 1:k) {
    curval = index[(1+foldbreaks[fold]):(foldbreaks[fold+1])]
    cv.rf.fit = randomForest(obesity ~ ., data = train_df[-curval,], mtry = i)
    cv.rf.pred = predict(cv.rf.fit, newdata = train_df[curval,])
    cv.tab = table(cv.rf.pred, train_df$obesity[curval])
    fold.acc = (cv.tab[1] + cv.tab[4]) / sum(cv.tab)
  }
  cv.acc.k[i] = mean(fold.acc)
}

which.max(cv.acc.k) # 4; CV acc = 0.9289941
plot(cv.acc.k, type = "b")

rf.obesity2 = randomForest(obesity ~., data = final_df, subset = train, mtry = 4, importance = TRUE)
rf.pred = predict(rf.obesity2, newdata = final_df[-train,])
table(predict = rf.pred, truth = y.test)
rfAcc = (207 + 181) / 421 # 0.92162

importance(rf.obesity2)
varImpPlot(rf.obesity2)

### XGBoost ###
install.packages("xgboost")
install.packages("caret")
library(xgboost)
library(caret)

train_control = trainControl(method = "cv", number = 10, search = "grid")

set.seed(1)
gbmGrid <-  expand.grid(max_depth = c(3, 5, 7), 
                        nrounds = c(50, 100, 150),
                        eta = c(0.1, 0.3, 0.5),
                        gamma = c(0, 1, 2),
                        subsample = c(0.8, 1),
                        min_child_weight = c(0.5, 1, 1.5),
                        colsample_bytree = c(0.4, 0.6, 0.8))

# training a XGBoost Classification tree model while tuning parameters
model = train(obesity ~., data = final_df[train,], method = "xgbTree", trControl = train_control, tuneGrid = gbmGrid, metric = "Accuracy", maximize = TRUE)
print(model)

# CV Accuracy = 0.9186981
model$bestTune

xgb.pred = predict(model, newdata = final_df[-train,])
table(predict = xgb.pred, truth = y.test)
xgbRecall = 182 / (182+15) # 0.92386
xgbSpec = 207 / (207 + 17) # 0.92411
xgbAcc = (207 + 182) / 421 # 0.92399

# refit params with xgboost()
xgb <- xgboost(data = data.matrix(train.X), 
               label = as.numeric(train.Y) - 1,
               max_depth = 7, 
               nrounds = 100, 
               eta = 0.5,
               subsample = 1,
               colsample_bytree = 0.6,
               eval_metric = "error",
               objective = "binary:logistic")
               
mat = xgb.importance(feature_names = colnames(train.X), model = xgb) 

# Feature Importance
xgb.plot.importance(mat[1:10,])
title(main = "Feature Importance", xlab = "Gain", ylab = "Variables")
