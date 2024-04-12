#Joining all data sets together
library(readr)
library(tidyverse)
data_file = "C://Users/jacob/OneDrive/Documents/ECO-395M-Final-Project/Modified_data"
final_joined <- read_csv("C://Users/jacob/OneDrive/Documents/ECO-395M-Final-Project/Modified_data/final_joined.csv") %>% 
  select(-1) %>%
  rename(Month = Month.x,
         Year = Year.x)
UR_data = read_csv("C://Users/jacob/OneDrive/Documents/ECO-395M-Final-Project/Modified_data/state_UR_data.csv")%>% 
  select(-1) %>%
  mutate(Month = month(as.Date(DATE)),
         Year = year(as.Date(DATE)))
Temp_data = read_csv("C://Users/jacob/OneDrive/Documents/ECO-395M-Final-Project/Modified_data/state_temp_data.csv")%>% 
  select(-1) %>%
  mutate(Month = month(as.Date(Date, format = "%Y%m")),
         Year = year(as.Date(Date, format = "%Y%m")))
gen_monthly = read_csv("C://Users/jacob/OneDrive/Documents/ECO-395M-Final-Project/Modified_data/gen_monthly.csv")%>% 
  select(-1) %>%
  rename(Month = MONTH,
         Year = YEAR,
         State = STATE)
final_monthly_join = inner_join(final_joined, gen_monthly, by = c("Year", "Month", "State"))
final_UR_join = inner_join(final_monthly_join, UR_data, by = c("Year", "Month", "State")) %>%
  select(-26) %>%
  rename(Date = DATE.x)
final_temp_join = inner_join(final_UR_join, UR_data, by = c("Year", "Month", "State")) %>%
  select(-26, -27) %>%
  rename(UR = UR_Rate.y)
final_data = final_temp_join
write.csv(final_data, "C://Users/jacob/OneDrive/Documents/ECO-395M-Final-Project/Modified_data/final_data.csv")