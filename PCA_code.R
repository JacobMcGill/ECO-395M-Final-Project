library(rpart)
library(randomForest)
library(gbm)
library(rpart.plot)
library(Metrics)
library(rsample)
library(pdp)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(rsample)
library(mosaic)
library(caret)
library(modelr)


# Converting energy data to PCA
baseline = read_csv("C://Users/jacob/OneDrive/Documents/ECO-395M-Final-Project/Modified_data/final_data.csv")
pre_pca_total <- baseline %>% 
  select(-1:-5, -9:-10)
pre_pca_gen <- baseline %>% 
  select(16:26)
state_data = baseline %>%
  select (2:5,9) 
#Note to self: Be sure not to include date column in regressions. 
#Lets get started by taking the pca of everyting numeric
PCA_total = prcomp(pre_pca, scale=TRUE, rank=5)
plot(PCA_total)
summary(PCA_total)
#Look at loadings of PCA
gen_summary = PCA_total$rotation %>%
  as.data.frame() %>%
  rownames_to_column('Var')
gen_summary %>%
  select(Var, PC1) %>%
  arrange(desc(PC1))
gen_summary %>%
  select(Var, PC2) %>%
  arrange(desc(PC2))
gen_summary %>%
  select(Var, PC3) %>%
  arrange(desc(PC3))
gen_summary %>%
  select(Var, PC4) %>%
  arrange(desc(PC4))
gen_summary %>%
  select(Var, PC5) %>%
  arrange(desc(PC5))
#Now we're going to merge PCA data
state_pca_total = merge(state_data, PCA_total$x[,1:5], by="row.names") %>%
  select(-1) %>%
  arrange(desc(Month), desc(Year), State)
#Take summary data for regression
state_pca_summary = state_pca_total %>%
  group_by(State) %>%
  summarize(PC1 = mean(PC1),
            PC2 = mean(PC2),
            PC3 = mean(PC3),
            PC4= mean(PC4),
            PC5 = mean(PC5))
ggplot(state_pca_summary) + 
  geom_col(aes(x=reorder(State, PC1), y=PC1)) + 
  coord_flip()
ggplot(state_pca_summary) + 
  geom_col(aes(x=reorder(State, PC2), y=PC2)) + 
  coord_flip()
ggplot(state_pca_summary) + 
  geom_col(aes(x=reorder(State, PC3), y=PC3)) + 
  coord_flip()
ggplot(state_pca_summary) + 
  geom_col(aes(x=reorder(State, PC4), y=PC4)) + 
  coord_flip()
ggplot(state_pca_summary) + 
  geom_col(aes(x=reorder(State, PC5), y=PC5)) + 
  coord_flip()
#Now lets focus on just energy generation
PCA_gen = prcomp(pre_pca_gen, scale=TRUE, rank=5)
plot(PCA_gen)
summary(PCA_gen)
#Look at loadings of PCA
gen_summary = PCA_gen$rotation %>%
  as.data.frame() %>%
  rownames_to_column('Var')
gen_summary %>%
  select(Var, PC1) %>%
  arrange(desc(PC1))
gen_summary %>%
  select(Var, PC2) %>%
  arrange(desc(PC2))
gen_summary %>%
  select(Var, PC3) %>%
  arrange(desc(PC3))
gen_summary %>%
  select(Var, PC4) %>%
  arrange(desc(PC4))
gen_summary %>%
  select(Var, PC5) %>%
  arrange(desc(PC5))
#Now we're going to merge PCA data
state_pca_gen = merge(baseline, PCA_gen$x[,1:5], by="row.names") %>%
  select(-1:-2) %>%
  arrange(desc(Month), desc(Year), State)
#Take summary data for regression
state_pca_gen_summary = state_pca_gen %>%
  group_by(State) %>%
  summarize(PC1 = mean(PC1),
            PC2 = mean(PC2),
            PC3 = mean(PC3),
            PC4= mean(PC4),
            PC5 = mean(PC5))
ggplot(state_pca_gen_summary) + 
  geom_col(aes(x=reorder(State, PC1), y=PC1)) + 
  coord_flip()
ggplot(state_pca_gen_summary) + 
  geom_col(aes(x=reorder(State, PC2), y=PC2)) + 
  coord_flip()
ggplot(state_pca_gen_summary) + 
  geom_col(aes(x=reorder(State, PC3), y=PC3)) + 
  coord_flip()
ggplot(state_pca_gen_summary) + 
  geom_col(aes(x=reorder(State, PC4), y=PC4)) + 
  coord_flip()
ggplot(state_pca_gen_summary) + 
  geom_col(aes(x=reorder(State, PC5), y=PC5)) + 
  coord_flip()
# Now we will move onto random forest
rf_baseline = baseline %>%
  select(-1,-5, -10) %>%
  rename(Thousand_dollars = `Thousand Dollars`,
         oil_emp = `oil_emp(thousands)`,
         US_Oil_Prod = `U.S. Field Production of Crude Oil Thousand Barrels per Day`,
         Lab_part = `Labor Force Participation`,
         Employ_rate = `Employment-Population Rate`,
         UR = `Unemployment Rate`)
energy_split =  initial_split(rf_baseline, prop=0.8)
energy_train = training(energy_split)
energy_test  = testing(energy_split)
energy.forest = randomForest(`Cents/kWh` ~ Year + Month + State + Thousand_dollars + 
                               Megawatthours + Count + oil_emp 
                             + US_Oil_Prod + Lab_part + Employ_rate + UR + coal_gen + hydro_gen 
                             + gas_gen + other_gen + petro_gen + solar_gen + biomass_gen
                             + wind_gen + nuclear_gen + other_gas_gen 
                             + pump_gen + Temp + Temp_Anomaly, data= energy_train, ntree = 1000, mtry = 5, importance = TRUE)
modelr::rmse(energy.forest, energy_test)
#Now lets move to PCA applied to most of the dataset
PCA_energy_split = initial_split(state_pca_total, prop = 0.8)
PCA_energy_train = training(PCA_energy_split)
PCA_energy_test = testing(PCA_energy_split)
PCA_energy.forest = randomForest(`Cents/kWh` ~ PC1 + PC2 + PC3 + PC4 + PC5 
                                 + State + Month + Year, data= PCA_energy_train, ntree = 1000, mtry = 5, importance = TRUE)
modelr::rmse(PCA_energy.forest, PCA_energy_test)

# Do PCA applied to only power generation
PCA_gen = state_pca_gen %>%
  select(-4, -9) %>%
  rename(Thousand_dollars = `Thousand Dollars`,
         oil_emp = `oil_emp(thousands)`,
         US_Oil_Prod = `U.S. Field Production of Crude Oil Thousand Barrels per Day`,
         Lab_part = `Labor Force Participation`,
         Employ_rate = `Employment-Population Rate`,
         UR = `Unemployment Rate`)
  
PCA_gen_split = initial_split(PCA_gen, prop = 0.8)
PCA_gen_train = training(PCA_gen_split)
PCA_gen_test = testing(PCA_gen_split)
PCA_gen.forest = randomForest(`Cents/kWh` ~ PC1 + PC2 + PC3 + PC4 + PC5 
                              + Year + Month + State + Thousand_dollars + 
                                Megawatthours + Count + oil_emp 
                              + US_Oil_Prod + Lab_part + Employ_rate + UR, data= PCA_gen_train, ntree = 1000, mtry = 5, importance = TRUE)
modelr::rmse(PCA_gen.forest, PCA_gen_test)
print(names(energy_train))
