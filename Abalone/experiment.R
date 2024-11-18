# Abalone

source("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Abalone/config.R")
library(devtools)
options("install.lock"=FALSE)
devtools::install("/Users/ylvasofietollefsen/Documents/Uio/Master/GMJMCMC")
library(FBMS)
library(randomForest)
library(dplyr)

set.seed(2024)

# Fix data
# df <- read.csv("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Abalone/abalone.csv")
# df["Sex_F"] <- as.numeric(df$Sex=="F")
# df["Sex_M"] <- as.numeric(df$Sex=="M")
# df <- df[-1]
# df <- as.data.frame(cbind(Rings = df[,8], df[,-8]))
# sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.75,0.25))
# train <- df[sample, ]
# test  <- df[!sample, ]

train <- read.csv("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Abalone/train.csv")
test <- read.csv("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Abalone/test.csv")
dim(df)
# Result csv
now <-format(Sys.time(), "%Y-%m-%d_%H:%M")
Results <- paste("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Kepler/","results_",now,".csv", sep="")

# Simple checks
common_rows <- inner_join(train, test, by = names(train))
if (nrow(common_rows) != 0) {
  stop("Error: There are common rows between training and testing sets.")
}

# Name of each experiment, same order as in thesis table
experiment_names <- c("S1","S2","S3","S4","S5","S6","P1","P2","P3","P4","P5","P6")

# Running the experiments
for (ex in c(1:12)){
  
  # To specify in model
  chains <- experiment_config$B[ifelse(ex <= 6, 1, 2)]
  P <- experiment_config$P[ifelse(ex <= 6, 1, 2)]
  ninit <- experiment_config$N_init
  nfinal <- experiment_config$N_final
  transforms <- experiment_config$transforms
  
  # Fix params and probs
  probs <- gen.probs.gmjmcmc(transforms)
  params <- gen.params.gmjmcmc(train)
  if (ex %in% c(1,2,7,8)){
    probs$gen <- experiment_config$probs[[1]]
  } else if (ex %in% c(3,4,9,10)){
    probs$gen <- experiment_config$probs[[2]]
  } else if (ex %in% c(5,6,11,12)){
    probs$gen <- experiment_config$probs[[3]]
  }
  params$feat$pop.max <- experiment_config$Q[((ex-1) %% 2)+1]
  params$feat$D <- experiment_config$D
  params$feat$L <- experiment_config$L
  params$feat$esp <- experiment_config$eps
  
  
  # Run each experiment count times
  for (number in c(1:experiment_config$count)){
    # The model
    if (chains != 1){
      time_taken <- system.time({
        model <- fbms(formula = Rings ~ ., runs = chains, cores = chains, data = train, 
                      transforms = transforms, method = "gmjmcmc.parallel", probs = probs,
                      params = params, P = P, N.init = ninit, N.final = nfinal)
      })
    } else {
      time_taken <- system.time({
        model <- fbms(formula = Rings ~ ., data = train, transforms = transforms,
                      method = "gmjmcmc", probs = probs, params = params, P = P, 
                      N.init = ninit, N.final = nfinal)
      })
    }

    summary <- summary(model, tol=0.1, pop="best")
    features <- summary$feats.strings

    # Three most important features 
    feature1 <- if (length(features) > 0) features[1] else "NA"
    feature2 <- if (length(features) > 1) features[2] else "NA"
    feature3 <- if (length(features) > 2) features[3] else "NA"

    # MAE 
    preds <- predict(model,test[,-1])$aggr$mean
    mae <- mean(abs(preds-test$Rings))
    
    # RMSE
    rmse <- sqrt(mean((preds-test$Rings)^2))
    
    # Correlation
    cor <- cor(preds, test$Rings, use = "complete.obs")
    
    # Time elapsed
    elapsed_time <- time_taken["elapsed"]
    
    
    # Write to csv
    current_results <- data.frame(
      Experiment = experiment_names[ex],
      Run = number,
      Correlation = cor,
      MAE = mae,
      RMSE = rmse,
      Time = elapsed_time,
      FirstFeature = feature1,
      SecondFeature = feature2,
      ThirdFeature = feature3
    )
    if (file.exists(Results)) {
      write.table(current_results, Results, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
    } else {
      write.csv(current_results, Results, row.names = FALSE)
    }
  }
}

# Random forest for comparison
model_rf <- randomForest(MajorAxis ~ ., data = train, ntree = 100, mtry = 3, importance = TRUE)
# Make predictions on the test set
predictions_rf <- predict(model_rf, newdata = test[,-1])

# Calculate the sum of squared errors (SSE) for the Random Forest model
mae_rf <- mean(abs(predictions_rf - test$MajorAxis))

# RMSE
rmse <- sqrt(mean((predictions_rf-test$Rings)^2))

# Correlation
cor <- cor(predictions_rf, test$Rings, use = "complete.obs")


current_results <- data.frame(
  Experiment = "Randomforest",
  Run = 1,
  Correlation = cor,
  MAE = mae_rf,
  RMSE = rmse,
  Time = "NA",
  FirstFeature = "NA",
  SecondFeature = "NA",
  ThirdFeature = "NA"
)
if (file.exists(Results)) {
  write.table(current_results, Results, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
} else {
  write.csv(current_results, Results, row.names = FALSE)
}
