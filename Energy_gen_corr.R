# Establishing relationship between energy generation and energy prices.
energy_gen_data = read_csv("C://Users/jacob/OneDrive/Documents/ECO-395M-Final-Project/Modified_data/final_data.csv") %>% 
  select(-1) 
energy_gen_yearly = energy_gen_data %>%
  group_by(Year, State) %>%
  summarize(energy_gen = mean(sum(coal_gen + hydro_gen + gas_gen + other_gen + petro_gen + 
                              solar_gen +
                              biomass_gen + wind_gen + nuclear_gen + other_gas_gen)),
            `Cents/kWh` = mean(`Cents/kWh`))
ggplot(energy_gen_yearly) +
  (geom_point(aes(x=energy_gen, y = `Cents/kWh`)))
avg_corr = cor.test(energy_gen_yearly$energy_gen, energy_gen_yearly$`Cents/kWh`)
avg_corr_estimate = cor(energy_gen_yearly$energy_gen, energy_gen_yearly$`Cents/kWh`)
avg_corr_p_value = avg_corr$p.value

coal_corr = cor.test(energy_gen_data$coal_gen, energy_gen_data$`Cents/kWh`)
coal_estimate = cor(energy_gen_data$coal_gen, energy_gen_data$`Cents/kWh`)
coal_p_value = coal_corr$p.value

hydro_corr = cor.test(energy_gen_data$hydro_gen, energy_gen_data$`Cents/kWh`)
hydro_estimate = cor(energy_gen_data$hydro_gen, energy_gen_data$`Cents/kWh`)
hydro_p_value = hydro_corr$p.value

gas_corr = cor.test(energy_gen_data$gas_gen, energy_gen_data$`Cents/kWh`)
gas_estimate = cor(energy_gen_data$gas_gen, energy_gen_data$`Cents/kWh`)
gas_p_value = gas_corr$p.value

other_corr = cor.test(energy_gen_data$other_gen, energy_gen_data$`Cents/kWh`)
other_estimate = cor(energy_gen_data$other_gen, energy_gen_data$`Cents/kWh`)
other_p_value = other_corr$p.value

petro_corr = cor.test(energy_gen_data$petro_gen, energy_gen_data$`Cents/kWh`)
petro_estimate = cor(energy_gen_data$petro_gen, energy_gen_data$`Cents/kWh`)
petro_p_value = petro_corr$p.value

solar_corr = cor.test(energy_gen_data$solar_gen, energy_gen_data$`Cents/kWh`)
solar_estimate = cor(energy_gen_data$solar_gen, energy_gen_data$`Cents/kWh`)
solar_p_value = solar_corr$p.value

biomass_corr = cor.test(energy_gen_data$biomass_gen, energy_gen_data$`Cents/kWh`)
biomass_estimate = cor(energy_gen_data$biomass_gen, energy_gen_data$`Cents/kWh`)
biomass_p_value = biomass_corr$p.value

wind_corr = cor.test(energy_gen_data$wind_gen, energy_gen_data$`Cents/kWh`)
wind_estimate = cor(energy_gen_data$wind_gen, energy_gen_data$`Cents/kWh`)
wind_p_value = wind_corr$p.value

nuclear_corr = cor.test(energy_gen_data$nuclear_gen, energy_gen_data$`Cents/kWh`)
nuclear_estimate = cor(energy_gen_data$nuclear_gen, energy_gen_data$`Cents/kWh`)
nuclear_p_value = nuclear_corr$p.value

other_gas_corr = cor.test(energy_gen_data$other_gas_gen, energy_gen_data$`Cents/kWh`)
other_gas_estimate = cor(energy_gen_data$other_gas_gen, energy_gen_data$`Cents/kWh`)
other_gas_p_value = other_gas_corr$p.value
Energy_correlation = matrix(c(avg_corr_estimate, avg_corr_p_value,
                             coal_estimate, coal_p_value,
                             hydro_estimate, hydro_p_value,
                             gas_estimate, gas_p_value,
                             other_estimate, other_p_value,
                             petro_estimate, petro_p_value,
                             solar_estimate, solar_p_value,
                             biomass_estimate, biomass_p_value,
                             wind_estimate, wind_p_value,
                             nuclear_estimate, nuclear_p_value,
                             other_gas_estimate, other_gas_p_value),
                            nrow = 11,
                            ncol = 2,
                            byrow = TRUE)
colnames(Energy_correlation) = c("Estimated Correlation", "P-Value")
rownames(Energy_correlation) = c("Average Energy Production", "Coal",
                                "Hydro", "Gas","Other", "Petro", "Solar", 
                                "Biomass", "Wind" , "Nuclear", "Other Gas")
Energy_correlation
#Find a negative and statistically significant relationship between energy generation and electricity price
# Now lets do oil employment
oil_empp_corr = cor.test(energy_gen_data$`oil_emp(thousands)`, energy_gen_data$`Cents/kWh`)




