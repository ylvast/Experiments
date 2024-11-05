source("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Kepler/config.R")
source("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Kepler/helpers.R")
source("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Kepler/metrics.R")
library(devtools)
options("install.lock"=FALSE)
devtools::install("/Users/ylvasofietollefsen/Documents/Uio/Master/GMJMCMC")
library(FBMS)

Results <- "/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Kepler/experiment_results.csv"
train <- read.csv("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Kepler/train.csv")[,-1]
test <- read.csv("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Kepler/test.csv")[,-1]

# Result dataframe
results <- data.frame(Experiment = character(), Run = integer(), TP = logical(), Actual_TP = logical(), Count_FP = integer(), Correlation = numeric(), Count_P = integer(), Time = numeric())

# Name of each experiment, same order as in thesis table
experiment_names <- c("S1","S2","S3","S4","S5","S6","P1","P2","P3","P4","P5","P6")
summary(train)

# Running the experiments
for (ex in c(1:12)){
  
  # Fix params and probs
  probs <- gen.probs.gmjmcmc(transforms)
  params <- gen.params.gmjmcmc(train)
  probs$gen <- experiment_config$probs[[((ex-1) %% 3)+1]]
  params$feat$pop.max <- experiment_config$Q[((ex-1) %% 2)+1]
  params$feat$D <- experiment_config$D
  params$feat$L <- experiment_config$L
  params$feat$esp <- experiment_config$eps
  
  # To specify in model
  chains <- experiment_config$B[ifelse(ex <= 6, 1, 2)]
  P <- experiment_config$P[ifelse(ex <= 6, 1, 2)]
  method <- ifelse(chains == 1, "gmjmcmc", "gmjmcmc.parallel")
  ninit <- experiment_config$N_init
  nfinal <- experiment_config$N_final
  transforms <- experiment_config$transforms
  
  # Run each experiment count times
  for (number in c(1:experiment_config$count)){
    # The model
    time_taken <- system.time({
      model <- fbms(formula = MajorAxis ~ ., data = train, transforms = transforms,
                    method = method, loglik.pi = gaussian.loglik,
                    loglik.alpha = gaussian.loglik.alpha, probs = probs,
                    params = params, P = P, N.init = ninit, N.final = nfinal)
    })
    summary <- summary(model, tol = 0.1)
    features <- summary$feats.strings
    
    # For results
    list_tp <- any_tp(col_number=dim(train[,2:ncol(train)])[2], features)
    count_tp <- length(list_tp) # Number of true positives
    tp <- count_tp>0 # If true positive in model
    actual_tp <- length(any_tp(col_number=dim(train[,2:ncol(train)])[2], features, kepler_feature=c("troot(x4*x4*x6)")))>0 # If the actual positive is in the model 
    fp <- length(features) - count_tp # Number of false positives
    if (count_tp==0){
      cor <- average_correlation(test,features) # Average correlation false positives
    } else{
      cor <- average_correlation(test,features[-list_tp]) # Average correlation false positives
    }
    elapsed_time <- time_taken["elapsed"]
    
    # Write to csv
    current_results <- data.frame(
      Experiment = experiment_names[ex],
      Run = number,
      TP = tp,
      Actual_TP = actual_tp,
      Count_FP = fp,
      Correlation = cor,
      Count_P = length(features),
      time = elapsed_time
    )
    if (file.exists(Results)) {
      write.table(current_results, Results, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
    } else {
      write.csv(current_results, Results, row.names = FALSE, col.names = TRUE)
    }
  }
}