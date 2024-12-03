
date <- "2024-11-19_09_41"
#results <- read.csv(paste("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Kepler/","results_",date,".csv", sep=""))
results <- read.csv("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Kepler/30_Single_200_pop_noise.csv")
#results <- read.csv(paste("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Kepler/","results_noise",date,".csv", sep=""))
results <- read.csv("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Kepler/30_parallel_50_pop_noise.csv")



#colnames <- c("S1","S2","S3","S4","S5","S6","P1","P2","P3","P4","P5","P6")
colnames <- c("S1","S2","S3","S4","S5","S6")
colnames <- c("P1","P2","P3","P4","P5","P6")
rownames <- c("Pow","F1","F2","F3","F4","Count_P","Count_FP","FDR","Correlation_P","Correlation_FP","MAE","Time")
Metrics <- data.frame(matrix(ncol = length(colnames), nrow = length(rownames)))
colnames(Metrics) <- colnames
rownames(Metrics) <- rownames

for (experiment in colnames) {
  exp_df <- results[results$Experiment==experiment,]
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
  Metrics["MAE",experiment] <- mean(as.numeric(exp_df$MAE), na.rm = TRUE)
  Metrics["Time",experiment] <- mean(as.numeric(exp_df$Time))
}

Metrics
