
# Returns number of true positives
any_tp <- function (col_number, features, kepler_feature = c("troot(x4*x4*x6)","troot(x4*x4*x9*x9)","troot(x4*x4*x9)","troot(x4*x4*x7)")) {
  correct_features <- c()
  data <- matrix(runif(1000*col_number,min=0.1, max=10000), nrow = 1000, ncol = col_number)
  colnames(data) <- paste0("x", 1:col_number)
  all_features <- c(kepler_feature,features)
  calculated <- matrix(NA, nrow(data), length(all_features))
  for (f in seq_along(all_features)) {
    calculated[, f] <- eval(parse(text = all_features[f]), envir = as.data.frame(data))
  }
  for (f in seq_along(features)) {
    current_values <- calculated[,f+length(kepler_feature)]
    for (c in seq_along(kepler_feature)) {
      correct_values <- calculated[,c]
      if (isTRUE(all.equal(current_values, correct_values, tolerance = 1e-6))){
        if(!(f %in% correct_features)){
          correct_features <- c(f,correct_features)
        }
      }
    }
  }
  return(correct_features)
}

average_correlation <- function (test, features){
  X <- test[,-1]
  all_features <- c("troot(x4*x4*x6)", features)
  corr_df <- matrix(NA, nrow(X), length(all_features))
  colnames(X) <- paste0("x", 1:ncol(X))
  colnames(corr_df) <- all_features
  for (f in seq_along(all_features)) {
    corr_df[, f] <- eval(parse(text = all_features[f]), envir = as.data.frame(X))
  }
  corr_df <- cor(corr_df)
  return(mean(corr_df[,1][-1]))
}
