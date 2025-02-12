library(FBMS)
test <- read.csv("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Housing/test.csv")
features <- read.csv("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Housing/Extended/Housing_results_single/11/features_11.csv")

df_s2 <- features[features$Experiment=="S2",]
for (feature in df_s2$Feature) {
  print(feature)
}

sigmoid(test$LSTAT)
min(exp_dbl(test$LSTAT))
min(exp_dbl(test$PTRATIO))
troot(test$INDUS)

