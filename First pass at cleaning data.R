library(readxl)
library(tidyverse)
library(dplyr)
Energy_data <- read_excel("C:/Users/jacob/Downloads/HS861M 2010-.xlsx")
Oil_Employment_data <- read.csv("C:/Users/jacob/Downloads/CES1021100001.csv")
US_Crude_Oil <- read_csv("C:/Users/jacob/Downloads/U.S._Field_Production_of_Crude_Oil.csv", skip = 4)
# Cleaning the data and focusing just on totals
Energy_data = Energy_data[-c(1),-c(5:20)]
names(Energy_data) = Energy_data[1,]
Energy_data = Energy_data[-c(1),]
Energy_data = Energy_data %>%
  mutate(DATE = as.Date(paste(Year, Month, 01, sep = "-")))
Oil_Employment_data = Oil_Employment_data %>%
  mutate(DATE = as.Date(DATE))
US_Crude_Oil = US_Crude_Oil %>%
  mutate(DATE = as.Date(paste0(substring(Month, 1, 3), "01", substring(Month, 5)), format = "%b %d %Y"))
#Now work on joing data
joined = inner_join(Energy_data, Oil_Employment_data, by = "DATE")
joined = subset((left_join(joined, US_Crude_Oil, by = "DATE")),select = -c(Month.y))  
