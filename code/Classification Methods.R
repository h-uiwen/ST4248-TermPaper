### ST4248 Term Paper ###
### Classification Methods ###

# train/test split (80/20)
0.8 * nrow(final_df) # 1684
set.seed(1)
train = sample(2105, 1684)
y.test = final_df$obesity[-train]

### Logistic Regression ###
glm.fit = glm(obesity ~., data = final_df[train,], family = binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, final_df[-train,], type = "response")

# set .5 as threshold first 
glm.pred = rep("no", 421)
glm.pred[glm.probs > .5] = "yes"
table(glm.pred, y.test)

glmAcc = (147 + 170) / 421 # 0.75297

# vary threshold
# threshold = c(.4, .5, .6, .7, .8)
# accVec = c()
# for (i in 1:length(threshold)) {
#   glm.pred = rep("no", 421)
#   glm.pred[glm.probs > threshold[i]] = "yes"
#   tn = (table(glm.pred, y.test))[1]
#   tp = (table(glm.pred, y.test))[4]
#   accVec[i] = (tn + tp) / 421
# }
# 
# # threshold = 0.4, acc = 0.7553444

# refit log reg without insignificant variables
glm.refit = glm(obesity ~ age + history + favc + fcvc + ncp + caec + scc + faf + mtrans,
                data = final_df[train,], family = binomial)
summary(glm.refit)
glm.probs2 = predict(glm.refit, final_df[-train,], type = "response")

set.seed(1)
k = 10
fold.error = rep(0, k)
n = length(train_df$obesity)
index = sample(n)
foldbreaks = c(0, floor(n/k * 1:k))
cv.glm.acc = c()

for(i in 1:length(threshold)) {
  for(fold in 1:k) {
    curval = index[(1+foldbreaks[fold]):(foldbreaks[fold+1])]
    cv.glm.fit = glm(obesity ~ age + history + favc + fcvc + ncp + caec + scc + faf + mtrans,
                    data = train_df[-curval,], family = binomial)
    cv.glm.prob = predict(cv.glm.fit, newdata = train_df[curval,], type = "response")
    cv.glm.pred = rep("no", length(curval))
    cv.glm.pred[cv.glm.prob > threshold[i]] = "yes"
    cv.tab = table(cv.glm.pred, train_df$obesity[curval])
    fold.acc = (cv.tab[1] + cv.tab[4]) / sum(cv.tab)
  }
  cv.glm.acc[i] = mean(fold.acc)
}

which.max(cv.glm.acc) # threshold = 0.4, acc = 0.8461538

glm.pred2 = rep("no", 421)
glm.pred2[glm.probs2 > 0.4] = "yes"
table(glm.pred2, y.test)
glmAcc = (136 + 183) / 421 # 0.7577197

### KNN ###
library(class)
train.X = (subset(final_df, select = -c(obesity)))[train,]
test.X = (subset(final_df, select = -c(obesity)))[-train,]
train.Y = final_df$obesity[train]  

# k = 1
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k=1)
table(knn.pred, y.test)

### SVM ###
library(e1071)

# linear 
set.seed(1)
tune.out = tune(svm, obesity ~., data = final_df[train,], kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10)))
summary(tune.out) # cost = 0.1, CV misclassification rate = 0.2114363

# radial
set.seed(1)
tune.out2 = tune(svm, obesity ~., data = final_df[train,], kernel = "radial", ranges = list(cost = c(0.1, 1, 10, 100, 1000), gamma = c(0.1, 0.5, 1, 3, 5)))
summary(tune.out2) # cost = 10, gamma = 0.5, CV misclassification rate = 0.09800296

# poly
set.seed(1)
tune.out3 = tune(svm, obesity ~., data = final_df[train,], kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 50), degree = c(1, 2, 3, 4, 5)))
summary(tune.out3) # cost = 50, degree = 5, CV misclassification rate = 0.1193998

# fit SVM with radial kernel
bestmod = tune.out2$best.model
svm.pred = predict(bestmod, final_df[-train,])
table(predict = svm.pred, truth = y.test)

recall = 172 / (172 + 25) # 0.87310
spec = 204 / (204 + 20) # 0.91071

# accuracy = 0.89311
