library(readr)
library(tidyverse)
temp_folder = "C://Users/jacob/OneDrive/Documents/ECO-395M-Final-Project/upload_data/Temp_data"
# List all CSV files in the directory
csv_files <- list.files(path = temp_folder, pattern = "\\.csv$", full.names = TRUE)


# Initialize an empty list to store data frames
dfs <- list()

# Read each CSV file, add a 'State' column, and append it to the list
for (csv_file in csv_files) {
  # Extract state name from the file name
  state_name <- tools::file_path_sans_ext(basename(csv_file))
  
  # Read CSV into a data frame using read_csv
  df <- read_csv(csv_file, skip = 4)
  
  # Add 'State' column using dplyr::mutate
  df <- df %>%
    mutate(State = state_name)
  
  # Append data frame to list
  dfs <- append(dfs, list(df))
}

# Merge all data frames into a single data frame
state_temp_data <- bind_rows(dfs)
write.csv(state_temp_data, "C://Users/jacob/OneDrive/Documents/ECO-395M-Final-Project/Modified_data/state_temp_data.csv")
