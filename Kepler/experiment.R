source("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Kepler/config.R")
source("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Kepler/helpers.R")
source("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Kepler/metrics.R")
library(devtools)
options("install.lock"=FALSE)
devtools::install("/Users/ylvasofietollefsen/Documents/Uio/Master/GMJMCMC")
library(FBMS)

now <-format(Sys.time(), "%Y-%m-%d_%H:%M")
Results <- paste("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Kepler/","results_",now,".csv", sep="")
train <- read.csv("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Kepler/train.csv")[,-1]
test <- read.csv("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Kepler/test.csv")[,-1]

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
    print("hei")
    probs$gen <- experiment_config$probs[[1]]
  } else if (ex %in% c(3,4,9,10)){
    print("yey")
    probs$gen <- experiment_config$probs[[2]]
  } else if (ex %in% c(5,6,11,12)){
    print("soup")
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
        model <- fbms(formula = MajorAxis ~ ., runs = chains, cores = chains, data = train, 
                      transforms = transforms, method = "gmjmcmc.parallel", probs = probs,
                      params = params, P = P, N.init = ninit, N.final = nfinal)
      })
    } else {
      time_taken <- system.time({
        model <- fbms(formula = MajorAxis ~ ., data = train, transforms = transforms,
                      method = "gmjmcmc", probs = probs, params = params, P = P, 
                      N.init = ninit, N.final = nfinal)
      })
    }
    summary <- summary(model, tol=0.1, pop="best")
    features <- summary$feats.strings
    
    # Count positives 
    list_tp <- any_tp(col_number=dim(train[,2:ncol(train)])[2], features)
    count_tp <- length(list_tp) # Number of true positives
    tp <- count_tp>0 # If true positive in model
    actual_tp <- length(any_tp(col_number=dim(train[,2:ncol(train)])[2], features, kepler_feature=c("troot(x4*x4*x6)")))>0 # Actual positives 
    fp <- length(features) - count_tp # Number of false positives
    
    # Correlations
    cor <- average_correlation(test,features) 
    if (count_tp==0){
      cor_fp <- average_correlation(test,features) # Average correlation false positives
    } else{
      cor_fp <- average_correlation(test,features[-list_tp]) # Average correlation false positives
    }
    
    # Three most important features 
    feature1 <- ifelse(length(features)>0,features[1],NULL)
    feature2 <- ifelse(length(features)>1,features[2],NULL)
    feature3 <- ifelse(length(features)>2,features[3],NULL)
    
    # Time elapsed
    elapsed_time <- time_taken["elapsed"]
    
    # RMSE 
    preds <- predict(model,test[,-1])$aggr$mean
    rmse <- sqrt(mean((preds-test$MajorAxis)^2))
    
    # Write to csv
    current_results <- data.frame(
      Experiment = experiment_names[ex],
      Run = number,
      TP = tp,
      Actual_TP = actual_tp,
      Count_FP = fp,
      Correlation = cor,
      Correlation_FP = cor_fp,
      RMSE = rmse,
      Count_P = length(features),
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