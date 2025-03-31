# Define the parent folder where the 30 subfolders are located
parent_folder <- "/uio/hume/student-u69/ylvasto/privat/Experiments/PMLBR/2025-03-24_10_54_FOURTH"

#folder <- "/uio/hume/student-u69/ylvasto/privat/Experiments/PMLBR/2025-03-10_17_16/8/results.csv"
#dat <- read.csv(folder)
#dat$Data
type <- "results"

# Generate folder names (assuming they are named "1", "2", ..., "30")
folder_names <- as.character(1:10)
folder_names_2 <- as.character(31:40)

# Combine both sets of folder names
all_folder_names <- c(folder_names,folder_names_2)

# Read and merge all CSV files
library(dplyr)

merged_data <- bind_rows(lapply(all_folder_names, function(folder) {
  file_path <- file.path(parent_folder, folder, "results.csv")
  #file_path <- file.path(parent_folder, folder, paste0("merged_",type,".csv"))
  #file_path <- file.path(parent_folder, paste0(type,"_", folder, ".csv"))
  if (file.exists(file_path)) {
    read.csv(file_path)
  } else {
    warning(paste("File not found:", file_path))
    NULL
  }
}))

# Save the merged data as a new CSV file
write.csv(merged_data, file = file.path(parent_folder, paste0("merged_",type,".csv")), row.names = FALSE)

# Print summary
print(dim(merged_data))  # Check the number of rows and columns

