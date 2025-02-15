# Define the parent folder where the 30 subfolders are located
parent_folder <- "/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Housing/Extended/2025-02-11_10_43"

type <- "results"

# Generate folder names (assuming they are named "1", "2", ..., "30")
folder_names <- as.character(1:30)

# Read and merge all CSV files
library(dplyr)

merged_data <- bind_rows(lapply(folder_names, function(folder) {
  file_path <- file.path(parent_folder, folder, paste0(type,"_", folder, ".csv"))
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

