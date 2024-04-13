library(readxl)
library(tidyverse)
library(dplyr)
#Test
employment_data <- read_csv("/Users/danieloliner/Downloads/monthlyemployment.csv")
joined_data <- read_csv("/Users/danieloliner/Downloads/joined_data.csv")

employment_data <- employment_data %>%
  mutate(DATE = as.Date(paste(Year, Month, "01", sep = "-")))

final_joined <- inner_join(joined_data, employment_data, by = c("DATE", "State" = "State Abbrev"))

final_joined <- final_joined %>%
  select(-Month, -Year.y, -State.y)

write.csv(final_joined, "/Users/danieloliner/Documents/GitHub/ECO-395M-Final-Project/Modified_data/final_joined.csv", row.names = FALSE)
