# Abalone

date <- "2024-11-13_17:29"
results <- read.csv(paste("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Abalone/","results_",date,".csv", sep=""))

colnames <- c("S1","S2","S3","S4","S5","S6","P1","P2","P3","P4","P5","P6")
rownames <- c("Correlation","MAE","RMSE","Time")
Metrics <- data.frame(matrix(ncol = length(colnames), nrow = length(rownames)))
colnames(Metrics) <- colnames
rownames(Metrics) <- rownames


for (experiment in colnames) {
  exp_df <- results[results$Experiment==experiment,]
  exp_df <- exp_df[rowSums(is.na(exp_df)) != ncol(exp_df), ]
  Metrics["Correlation",experiment] <- mean(exp_df$Correlation)
  Metrics["MAE",experiment] <- mean(exp_df$MAE, na.rm = TRUE)
  Metrics["RMSE",experiment] <- mean(exp_df$RMSE, na.rm = TRUE)
  Metrics["Time",experiment] <- mean(exp_df$Time)
}

Metrics

