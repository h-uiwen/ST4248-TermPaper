### ST4248 Term Paper ###
### Preliminary Analysis ###

### Exploratory Data Analysis ###
caec = df$caec
caecNo = sum((as.numeric(as.factor(df$obesity)) - 1)[caec == "no"])
caecSome = sum((as.numeric(as.factor(df$obesity)) - 1)[caec == "Sometimes"])
caecFreq = sum((as.numeric(as.factor(df$obesity)) - 1)[caec == "Frequently"])
caecAlw = sum((as.numeric(as.factor(df$obesity)) - 1)[caec == "Always"])
caecVec = c(caecNo, caecSome, caecFreq, caecAlw)
names = c("No", "Sometimes", "Frequently", "Always")
barplot(caecVec, names.arg = names, xlab = "caec", ylab = "Obesity", main = "Incidence of Obesity by Consumption Between Meals")

history = df$history
historyNo = sum((as.numeric(as.factor(df$obesity)) - 1)[history == "no"])
historyYes = sum((as.numeric(as.factor(df$obesity)) - 1)[history == "yes"])
histVec = c(historyNo, historyYes)
histNames = c("No", "Yes")
barplot(histVec, names.arg = histNames, xlab = "history", ylab = "Obesity", main = "Incidence of Obesity by Family History")

ncp = df$ncp
ncp1 = sum((as.numeric(as.factor(df$obesity)) - 1)[ncp == "1"])
ncp2 = sum((as.numeric(as.factor(df$obesity)) - 1)[ncp == "2"])
ncp3 = sum((as.numeric(as.factor(df$obesity)) - 1)[ncp == "3"])
ncpVec = c(ncp1, ncp2, ncp3)
ncpNames = c("1", "2", "3")
barplot(ncpVec, names.arg = ncpNames, xlab = "ncp", ylab = "Obesity", main = "Incidence of Obesity by Number of Main Meals")

faf = df$faf
faf0 = sum((as.numeric(as.factor(df$obesity)) - 1)[faf == "0"])
faf1 = sum((as.numeric(as.factor(df$obesity)) - 1)[faf == "1"])
faf2 = sum((as.numeric(as.factor(df$obesity)) - 1)[faf == "2"])
faf3 = sum((as.numeric(as.factor(df$obesity)) - 1)[faf == "3"])
fafVec = c(faf0, faf1, faf2, faf3)
fafNames = c("0", "1", "2", "3")
barplot(fafVec, names.arg = fafNames, xlab = "faf", ylab = "Obesity", main = "Incidence of Obesity by Frequency of Physical Activity")


### Best Subset Selection ###
# use bmi as response for best subset
temp_df = subset(df, select = -c(height, weight, weightclass, obesity))

### Best Subset Selection ###
library(leaps)
regfit.full = regsubsets(bmi ~ ., data = temp_df, nvmax = 21)
reg.summary = summary(regfit.full)
par(mfrow = c(1, 3))
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type="l")
which.max(reg.summary$adjr2) # 17
points(17, reg.summary$adjr2[17], col = "red", cex = 2, pch=20)
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = 'l')
which.min(reg.summary$cp) # 17
points(17, reg.summary$cp[17], col = "red", cex = 2, pch = 20)
plot(reg.summary$bic,xlab="Number of Variables", ylab="BIC",type='l')
which.min(reg.summary$bic) # 11
points(11, reg.summary$bic[11], col = "red", cex = 2, pch = 20)

regfit = regsubsets(bmi ~ ., data = temp_df, nvmax = 11)
summary(regfit)

final_df = subset(df, select = -c(bmi, height, weight, weightclass, smoke))
final_df$obesity = as.factor(final_df$obesity)


