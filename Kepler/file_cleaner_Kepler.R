path <- "/uio/hume/student-u69/ylvasto/privat/Experiments/Kepler/Extended/2025-02-26_16_32_parallel_fixed"
folder <- "36"
experiment <- "P2"

list_of_files <- c("features_1","features_2","features_3","features_4","merged_features","merged_results","results_1","results_2","results_3","results_4")

for (file in list_of_files) {
  # Construct the file path
  file_path <- file.path(path, folder, paste0(file, ".csv"))
  
  # Read the CSV file
  data <- read.csv(file_path)
  
  # Remove rows where Experiment equals experiment
  data_filtered <- subset(data, Experiment != experiment)
  
  # Write the filtered data back to the same file
  write.csv(data_filtered, file_path, row.names = FALSE)
}

merged_path <- file.path(path, "merged_results.csv")
merged <- read.csv(merged_path)
for (ex in c("P1", "P2", "P3", "P4", "P5", "P6")) {
  print(paste(ex, ":", dim(merged[merged$Experiment == ex, ])[1], "rows"))
}
