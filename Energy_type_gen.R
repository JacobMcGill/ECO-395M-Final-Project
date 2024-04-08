library(readxl)
library(tidyverse)


#Extract the sheets for 2018 through 2023 anf combine them into a single dataframe
gen_2018 = read_excel("C://Users/jacob/Downloads/Data_Mining_project/generation_monthly.xlsx", sheet = "2018_Final", col_names = FALSE, skip = 5)
gen_2019 = read_excel("C://Users/jacob/Downloads/Data_Mining_project/generation_monthly.xlsx", sheet = "2019_Final", col_names = FALSE, skip = 5)
gen_2020 = read_excel("C://Users/jacob/Downloads/Data_Mining_project/generation_monthly.xlsx", sheet = "2020_Final", col_names = FALSE, skip = 5)
gen_2021 = read_excel("C://Users/jacob/Downloads/Data_Mining_project/generation_monthly.xlsx", sheet = "2021_Final", col_names = FALSE, skip = 5)
gen_2022 = read_excel("C://Users/jacob/Downloads/Data_Mining_project/generation_monthly.xlsx", sheet = "2022_Final", col_names = FALSE, skip = 5)
gen_2023 = read_excel("C://Users/jacob/Downloads/Data_Mining_project/generation_monthly.xlsx", sheet = "2023_Preliminary",col_names = FALSE, skip = 5)
generation_monthly = rbind(gen_2018, gen_2019, gen_2020, gen_2021, gen_2022, gen_2023) %>%
  rename(YEAR = 1,
         MONTH = 2,
         STATE = 3,
         TYPE  = 4,
         ENERGY_SOURCE = 5,
         GEN = 6)
#Now extract each power type and return rows 
# Coal power
coal_gen <- generation_monthly %>%
  filter(TYPE == "Total Electric Power Industry" & ENERGY_SOURCE == "Coal") %>%
  rename(coal_gen = GEN)
coal_gen = coal_gen[-c(4:5)]
# Hydroelectric power
hydro_gen <- generation_monthly %>%
  filter(TYPE == "Total Electric Power Industry" & ENERGY_SOURCE == "Hydroelectric Conventional")%>%
  rename(hydro_gen = GEN)
hydro_gen = hydro_gen[-c(4:5)]
# Extracting gas power
Gas_gen <- generation_monthly %>%
  filter(TYPE == "Total Electric Power Industry" & ENERGY_SOURCE == "Natural Gas")%>%
  rename(gas_gen = GEN)
Gas_gen = Gas_gen[-c(4:5)]
# Extracting other power
other_gen <- generation_monthly %>%
  filter(TYPE == "Total Electric Power Industry" & ENERGY_SOURCE == "Other")%>%
  rename(other_gen = GEN)
other_gen = other_gen[-c(4:5)]
# Extracting petroleum power
petro_gen <- generation_monthly %>%
  filter(TYPE == "Total Electric Power Industry" & ENERGY_SOURCE == "Petroleum")%>%
  rename(petro_gen = GEN)
petro_gen = petro_gen[-c(4:5)]
# Extracting solar power
solar_gen <- generation_monthly %>%
  filter(TYPE == "Total Electric Power Industry" & ENERGY_SOURCE == "Solar Thermal and Pholtovaic")%>%
  rename(solar_gen = GEN)
solar_gen = solar_gen[-c(4:5)]
# Biomass power
biomass_gen <- generation_monthly %>%
  filter(TYPE == "Total Electric Power Industry" & ENERGY_SOURCE == "Other Biomass") %>%
  rename(biomass_gen = GEN)
biomass_gen = biomass_gen[-c(4:5)]
# Wind power
wind_gen <- generation_monthly %>%
  filter(TYPE == "Total Electric Power Industry" & ENERGY_SOURCE == "Wind")%>%
  rename(wind_gen = GEN)
wind_gen = wind_gen[-c(4:5)]
# Wood power
wood_gen <- generation_monthly %>%
  filter(TYPE == "Total Electric Power Industry" & ENERGY_SOURCE == "Wood and Wood Derived Fuels")%>%
  rename(wood_gen = GEN)
wood_gen = wood_gen[-c(4:5)]
# Nuclear generation
nuclear_gen <- generation_monthly %>%
  filter(TYPE == "Total Electric Power Industry" & ENERGY_SOURCE == "Nuclear")%>%
  rename(nuclear_gen = GEN)
nuclear_gen = nuclear_gen[-c(4:5)]
# Other gases
other_gas_gen <- generation_monthly %>%
  filter(TYPE == "Total Electric Power Industry" & ENERGY_SOURCE == "Other Gases")%>%
  rename(other_gas_gen = GEN)
other_gas_gen = other_gas_gen[-c(4:5)]
# Pumped generation
pumped_gen <- generation_monthly %>%
  filter(TYPE == "Total Electric Power Industry" & ENERGY_SOURCE == "Pumped Storage") %>%
  rename(pumped_gen = GEN)
pumped_gen = pumped_gen[-c(4:5)]

generation_monthly = left_join(generation_monthly, coal_gen, by = c("STATE"="STATE", "YEAR" = "YEAR", "MONTH" = "MONTH"))
generation_monthly = left_join(generation_monthly, hydro_gen, by = c("STATE"="STATE", "YEAR" = "YEAR", "MONTH" = "MONTH"))
generation_monthly = left_join(generation_monthly, Gas_gen, by = c("STATE"="STATE", "YEAR" = "YEAR", "MONTH" = "MONTH"))
generation_monthly = left_join(generation_monthly, other_gen, by = c("STATE"="STATE", "YEAR" = "YEAR", "MONTH" = "MONTH"))
generation_monthly = left_join(generation_monthly, petro_gen, by = c("STATE"="STATE", "YEAR" = "YEAR", "MONTH" = "MONTH"))
generation_monthly = left_join(generation_monthly, solar_gen, by = c("STATE"="STATE", "YEAR" = "YEAR", "MONTH" = "MONTH"))
generation_monthly = left_join(generation_monthly, biomass_gen, by = c("STATE"="STATE", "YEAR" = "YEAR", "MONTH" = "MONTH"))
generation_monthly = left_join(generation_monthly, wind_gen, by = c("STATE"="STATE", "YEAR" = "YEAR", "MONTH" = "MONTH"))
generation_monthly = left_join(generation_monthly, wood_gen, by = c("STATE"="STATE", "YEAR" = "YEAR", "MONTH" = "MONTH"))
generation_monthly = left_join(generation_monthly, nuclear_gen, by = c("STATE"="STATE", "YEAR" = "YEAR", "MONTH" = "MONTH"))
generation_monthly = left_join(generation_monthly, other_gas_gen, by = c("STATE"="STATE", "YEAR" = "YEAR", "MONTH" = "MONTH"))
generation_monthly = left_join(generation_monthly, pumped_gen, by = c("STATE"="STATE", "YEAR" = "YEAR", "MONTH" = "MONTH"))

generation_monthly = mutate_all(generation_monthly, ~ifelse(is.na(.), 0, .))
generation_monthly = generation_monthly %>%
  group_by(STATE, YEAR) %>%
  summarize(coal_gen = mean(coal_gen),
            hydro_gen = mean(hydro_gen),
            gas_gen = mean(gas_gen),
            other_gen = mean(other_gen),
            petro_gen = mean(petro_gen),
            solar_gen = mean(solar_gen),
            biomass_gen = mean(biomass_gen),
            wind_gen = mean(wind_gen),
            nuclear_gen = mean(nuclear_gen),
            other_gas_gen = mean(other_gas_gen),
            pump_gen = mean(pumped_gen))
write.csv(generation_monthly, "C://Users/jacob/Downloads/Data_Mining_project/ECO-395M-Final-Project/Modified_data/gen_monthly.csv")