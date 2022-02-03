# Workflow

# Read in datasets of country-specific use rate and the Loess curve of 
# access vs nets per capita
data <- prepare_data()
country_use_rate <- data$use_rate_by_country
loess_for_prediction <- data$loess_for_prediction
# Net half lives (median retention times) for each country
half_life_data <- data$half_life_data

# Convert net usage to annual nets distributed per capita for a selection of countries
result <- convert_usage_to_annual_nets_distributed(target_usage=seq(0,1,0.1),
                                                   use_rate_data=subset(country_use_rate,
                                                                        iso3 %in% c("AGO", "BEN", "NGA")),
                                                   half_life_data=subset(half_life_data,
                                                                         iso3 %in% c("AGO", "BEN", "NGA")))
# Convert net usage to annual nets distributed per capita with average estimates
result_average <- convert_usage_to_annual_nets_distributed(target_usage=seq(0,1,0.1),
                                                   use_rate_data=median(country_use_rate$use_rate),
                                                   half_life_data=median(half_life_data$half_life))
