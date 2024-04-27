library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(reshape2)

# Load the data 
data <- read_csv("/Users/danieloliner/Downloads/final_data.csv")

time_correlations <- data %>%
  group_by(Year, Month) %>%
  summarize(correlation = cor(`Unemployment Rate`, `Cents/kWh`, use = "complete.obs")) %>%
  ungroup() %>%
  arrange(Year, Month) %>%
  mutate(Time = as.Date(paste(Year, Month, "01", sep="-")))  # Creating a Date column for easier plotting

ggplot(time_correlations, aes(x = Time, y = correlation)) +
  geom_line() + 
  geom_point() +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(title = "Total Correlation Between Unemployment Rate and Electricity Costs",
       x = "Time",
       y = "Correlation") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "darkred", size = 1) +
  annotate("text", x = as.Date("2020-03-01"), y = 0.8, label = "Start of COVID-19 Pandemic", color = "darkred", size = 4, angle = 0, vjust = 1, fontface = "bold", hjust = -0.03)


# Potential Explanations for the jump in correlation at the onset of the pandemic: 
# Economic Disruptions: The pandemic caused significant disruptions in both the labor market and energy sector. The increase in correlation could indicate that these two variables were simultaneously affected by the pandemic’s economic shock.
# Policy Responses: Government interventions in response to the pandemic, such as stimulus packages or utility relief programs, could have influenced both unemployment rates and electricity prices, potentially leading to changes in their historical relationship.
# Demand Patterns: Unemployment can impact energy demand due to fewer people working and more people staying home. The change in electricity costs might reflect altered consumption patterns during lockdowns.
# Supply Chain and Production: The pandemic affected supply chains and production capabilities. Fluctuations in the cost of electricity might have occurred alongside changes in unemployment rates due to shutdowns in various industries.


