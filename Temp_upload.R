library(readr)
library(tidyverse)
temp_folder = "C://Users/jacob/Downloads/Data_Mining_project/Temp_data"
# List all CSV files in the directory
csv_files <- list.files(path = temp_folder, pattern = "\\.csv$", full.names = TRUE)


# Initialize an empty list to store data frames
dfs <- list()

# Read each CSV file, add a 'State' column, and append it to the list
for (csv_file in csv_files) {
  # Extract state name from the file name
  state_name <- tools::file_path_sans_ext(basename(csv_file))
  
  # Read CSV into a data frame using readr::read_csv
  df <- read_csv(csv_file, skip = 4)
  
  # Add 'State' column using dplyr::mutate
  df <- df %>%
    mutate(State = state_name)
  
  # Append data frame to list
  dfs <- append(dfs, list(df))
}

# Merge all data frames into a single data frame
merged_df <- bind_rows(dfs)