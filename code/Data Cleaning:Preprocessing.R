### ST4248 Term Paper ###

### Data Cleaning ###
df = read.csv("/Users/huiwen/Desktop/ObesityDataSet_raw_and_data_sinthetic.csv", header = TRUE)

# check for NA values
na.omit(df) # no NA values
# check for erroneous values (continued below)
bmi = c()
for (i in 1:nrow(df)) {
  height = df$Height[i]
  weight = df$Weight[i]
  bmi[i] = weight/height^2
}

df = cbind(df, bmi)

# rename columns
names(df) = tolower(names(df))
names(df)[5] = "history"
names(df)[17] = "weightclass"

### Data Preprocessing ###

# set up 2-class problem
obesity = c()
for (i in 1:nrow(df)) {
  weightclass = df$weightclass[i]
  lst = strsplit(weightclass, split = "_")
  if (sum(lst[[1]] == "Obesity") > 0) {
    obesity[i] = "yes"
  } else {
    obesity[i] = "no"
  }
}

filter = c()
for (i in 1:length(bmi)) {
  if (bmi[i] >= 30) {
    filter[i] = "yes"
  } else {
    filter[i] = "no"
  }
}

df = df[-which(filter != obesity),] # 5 erroneous rows
obesity = obesity[-which(filter != obesity)]
df = cbind(df, obesity)

length(obesity[obesity == "yes"]) # 970; 46% of data (balanced)

# floor/round values
df$age = floor(df$age)
df$fcvc = round(df$fcvc)
df$ncp = round(df$ncp)
df$ch2o = round(df$ch2o)
df$faf = round(df$faf)
df$tue = round(df$tue)

# as.factor
df$gender = as.factor(df$gender)
df$history = as.factor(df$history)
df$favc = as.factor(df$favc) # high cal
df$smoke = as.factor(df$smoke)
df$scc = as.factor(df$scc) # monitor cal
df$caec = as.factor(df$caec) # eat btwn meals
df$calc = as.factor(df$calc) # alc consumption
df$mtrans = as.factor(df$mtrans)

# integer encoding
# df$gender = ifelse(df$gender == "Female", 1, 0)
# df$history = ifelse(df$history == "yes", 1, 0) 
# df$favc = ifelse(df$favc == "yes", 1, 0) # high cal
# df$smoke = ifelse(df$smoke == "yes", 1, 0) 
# df$scc = ifelse(df$scc == "yes", 1, 0) # monitor cal
# # eat between meals
# for (i in 1:length(df$caec)) {
#   val = df$caec[i]
#   if (val == "no") {
#     df$caec[i] = 0
#   } else if (val == "Sometimes") {
#     df$caec[i] = 1
#   } else if (val == "Frequently") {
#     df$caec[i] = 2
#   } else {
#     df$caec[i] = 3
#   }
# }
# # alc consumption
# for (i in 1:length(df$calc)) {
#   val = df$calc[i]
#   if (val == "no") {
#     df$calc[i] = 0
#   } else if (val == "Sometimes") {
#     df$calc[i] = 1
#   } else if (val == "Frequently") {
#     df$calc[i] = 2
#   } else {
#     df$calc[i] = 3
#   }
# }

