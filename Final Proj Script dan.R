library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(reshape2)
library(glmnet)
library(rsample)


# Load the data 
data <- read_csv("/Users/danieloliner/Downloads/final_data.csv")

## Correlation Between Unemployment Rate and Energy Costs 

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


## Correlation Between Temperature and Energy Costs 


data <- data %>%
  mutate(Temp = as.numeric(Temp))

time_correlations_temp_energy <- data %>%
  group_by(Year, Month) %>%
  summarize(correlation = cor(Temp, `Cents/kWh`, use = "complete.obs")) %>%
  ungroup() %>%
  arrange(Year, Month) %>%
  mutate(Time = as.Date(paste(Year, Month, "01", sep="-")))

ggplot(time_correlations_temp_energy, aes(x = Time, y = correlation)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(title = "Correlation Between Temperature and Energy Costs Over Time",
       subtitle = "Data includes monthly average temperatures and energy costs",
       x = "Time",
       y = "Correlation") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

## Begin Model Building

split <- initial_split(data, prop = 0.8)
train_set <- training(split)
test_set <- testing(split)

## Baseline Linear Model 

baseline_model <- lm(`Cents/kWh` ~ Year + Month + State + `Thousand Dollars` + Megawatthours + 
                       Count + `oil_emp(thousands)` + `U.S. Field Production of Crude Oil Thousand Barrels per Day` + 
                       `Labor Force Participation` + `Employment-Population Rate` + 
                       `Unemployment Rate` + coal_gen + hydro_gen + gas_gen + 
                       other_gen + petro_gen + solar_gen + biomass_gen + 
                       wind_gen + nuclear_gen + other_gas_gen + pump_gen + 
                       Temp + Temp_Anomaly, 
                     data = train_set)

summary(baseline_model)

## Lasso Regression

X <- model.matrix(`Cents/kWh` ~ ., data = train_set)
Y <- train_set$`Cents/kWh`

# Perform cross-validated Lasso regression
cv <- cv.glmnet(X, Y, alpha = 1, nfolds = 10)

# Select the alpha value with the lowest mean cross-validated error
best_alpha <- cv$lambda.min

lasso_model <- glmnet(X, Y, alpha = 1, lambda = best_alpha)

final_lasso <- glmnet(X, Y, alpha = 1, lambda = best_alpha, standardize = TRUE)

# Summary of the final model
summary(final_lasso)


## Comparison of RMSEs 

baseline_predictions <- predict(baseline_model, newdata = test_set)
lasso_predictions <- predict(final_lasso, newx = model.matrix(`Cents/kWh` ~ ., data = test_set))

baseline_rmse <- sqrt(mean((baseline_predictions - test_set$`Cents/kWh`)^2))
lasso_rmse <- sqrt(mean((lasso_predictions - test_set$`Cents/kWh`)^2))

baseline_rmse
lasso_rmse