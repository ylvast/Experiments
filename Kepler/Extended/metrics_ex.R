
results <- read.csv("/uio/hume/student-u69/ylvasto/privat/Experiments/Kepler/Extended/2025-02-26_16_32_parallel_fixed/merged_results.csv")
feature_results <- read.csv("/uio/hume/student-u69/ylvasto/privat/Experiments/Kepler/Extended/2025-02-26_16_32_parallel_fixed/merged_features.csv")
source("/uio/hume/student-u69/ylvasto/privat/Experiments/Kepler/helpers.R")
library(FBMS)
results
summary(results)
dim(results)


s1 <- feature_results[feature_results$Experiment=="S1",]
s2 <- feature_results[feature_results$Experiment=="S2",]
s3 <- feature_results[feature_results$Experiment=="S3",]
s4 <- feature_results[feature_results$Experiment=="S4",]
s5 <- feature_results[feature_results$Experiment=="S5",]
s6 <- feature_results[feature_results$Experiment=="S6",]
list_of_s1 <- max_prob_tp(10,s1)
list_of_s2 <- max_prob_tp(10,s2)
list_of_s1
list_of_s2
list_of_s3 <- max_prob_tp(10,s3)
list_of_s4 <- max_prob_tp(10,s4)
list_of_s3
list_of_s4
list_of_s5 <- max_prob_tp(10,s5)
list_of_s6 <- max_prob_tp(10,s6)
list_of_s5
list_of_s6

colnames <- c("S1","S2","S3","S4","S5","S6","P1","P2","P3","P4","P5","P6")
colnames <- c("P1","P2","P3","P4","P5","P6")
rownames <- c("Pow","F1","F2","F3","F4","Count_P","Count_FP","FDR","Correlation_P","Correlation_FP","MAE","Time","Correlation_P_Med",
              "Correlation_P_Max","Correlation_P_Min","Correlation_FP_Med","Correlation_FP_Max","Correlation_FP_Min","MAE_Med","MAE_Max",
              "MAE_Min", "R_Med", "R_Min", "R_Max", "LMP_Med", "LMP_Min", "LMP_Max", "pos_Med","pos_Min","pos_Max")
Metrics <- data.frame(matrix(ncol = length(colnames), nrow = length(rownames)))
colnames(Metrics) <- colnames
rownames(Metrics) <- rownames

for (experiment in colnames) {
  exp_df <- results[results$Experiment==experiment,]
  features_df <- feature_results[feature_results$Experiment==experiment,]
  exp_df <- exp_df[rowSums(is.na(exp_df)) != ncol(exp_df), ]
  Metrics["Pow",experiment] <- mean(apply(exp_df[c("F1","F2","F3","F4")], 1, any))
  Metrics["F1",experiment] <- sum(as.logical(exp_df$F1))
  Metrics["F2",experiment] <- sum(as.logical(exp_df$F2))
  Metrics["F3",experiment] <- sum(as.logical(exp_df$F3)) 
  Metrics["F4",experiment] <- sum(as.logical(exp_df$F4))
  Metrics["Count_P",experiment] <- mean(as.integer(exp_df$Count_P)) 
  Metrics["Count_FP",experiment] <- mean(as.integer(exp_df$Count_FP))
  Metrics["FDR",experiment] <- mean(as.integer(exp_df$Count_FP)/as.integer(exp_df$Count_P))
  Metrics["Correlation_P",experiment] <- mean(as.numeric(exp_df$Correlation))
  Metrics["Correlation_FP",experiment] <- mean(as.numeric(exp_df$Correlation_FP), na.rm = TRUE)
  Metrics["Correlation_P_Med",experiment] <- median(as.numeric(exp_df$Correlation))
  Metrics["Correlation_FP_Med",experiment] <- median(as.numeric(exp_df$Correlation_FP), na.rm = TRUE)
  Metrics["Correlation_P_Max",experiment] <- max(as.numeric(exp_df$Correlation))
  Metrics["Correlation_FP_Max",experiment] <- max(as.numeric(exp_df$Correlation_FP), na.rm = TRUE)
  Metrics["Correlation_P_Min",experiment] <- min(as.numeric(exp_df$Correlation))
  Metrics["Correlation_FP_Min",experiment] <- min(as.numeric(exp_df$Correlation_FP), na.rm = TRUE)
  Metrics["MAE",experiment] <- mean(as.numeric(exp_df$MAE), na.rm = TRUE)
  Metrics["MAE_Med",experiment] <- median(as.numeric(exp_df$MAE), na.rm = TRUE)
  Metrics["MAE_Min",experiment] <- min(as.numeric(exp_df$MAE), na.rm = TRUE)
  Metrics["MAE_Max",experiment] <- max(as.numeric(exp_df$MAE), na.rm = TRUE)
  Metrics["LMP_Med",experiment] <- median(as.numeric(exp_df$LMP), na.rm = TRUE)
  Metrics["LMP_Min",experiment] <- min(as.numeric(exp_df$LMP), na.rm = TRUE)
  Metrics["LMP_Max",experiment] <- max(as.numeric(exp_df$LMP), na.rm = TRUE)
  Metrics["R_Med",experiment] <- median(as.numeric(exp_df$R2), na.rm = TRUE)
  Metrics["R_Min",experiment] <- min(as.numeric(exp_df$R2), na.rm = TRUE)
  Metrics["R_Max",experiment] <- max(as.numeric(exp_df$R2), na.rm = TRUE)
  list_of_margs <- max_prob_tp(10,features_df)
  Metrics["pos_Med",experiment] <- median(list_of_margs[list_of_margs>0.1], na.rm = TRUE)
  Metrics["pos_Min",experiment] <- min(list_of_margs[list_of_margs>0.1], na.rm = TRUE)
  Metrics["pos_Max",experiment] <- max(list_of_margs, na.rm = TRUE)
  Metrics["Time",experiment] <- mean(as.numeric(exp_df$Time))
}

features_df <- feature_results[feature_results$Experiment=="S1",]
max_prob_tp(10,features_df)
Metrics
list_of_margs

dim(results)
