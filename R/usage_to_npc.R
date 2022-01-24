### Convert bednet usage to nets-per-capita (NPC)

# Goal: produce country-specific loess functions to predict npc for a given (continuous) 
# input usage or access. Define a simple rule to extrapolate curves beyond 
# observed levels of usage access.

# For each country, what steady-state nets-per-capita is required to attain a given net use?
# Assumptions: 
# - Net use rates stays at 2019 levels for all countries
# - When converting from access to NPC, use the 2019 loess curve fit of access-NPC 
#   at the country-month level.
# - When determining the number of nets to distribute in each mass campaign, 
#   assume mass campaigns occur every three years.

# Function to input usage and return nets per capita for given country
find_npc <- function(target_usage, country_iso3="all", extrapolate_npc = "linear") {
  
  # Read in datasets of country-specific use rate and the Loess curve of 
  # access vs nets per capita
  country_use_rate <- read.csv("data/use_rate_by_country.csv")
  loess_for_prediction <- read.csv("data/access_vs_npc_loess.csv") 
  
  if(country_iso3[1]=="all") {
    country_iso3 <- country_use_rate$iso3
  }
  
  if(extrapolate_npc == "linear") {
    # Add datapoint of (1,1) to allow linear extrapolation of nets per capita
    # for access levels beyond those observed
    loess_for_prediction <- rbind(loess_for_prediction,
                                  data.frame(iso3=NA, month=NA, access=1, 
                                             percapita_nets = 1, loess_predicted_access=1))
    print("Access-nets per capita curve is extrapolated linearly beyond observed levels")
  } else {
    print("No extrapolation beyond observed access-nets per capita relationship - return NA")
  }
  
  max_usage <- country_use_rate[country_use_rate$iso3 %in% country_iso3,c("iso3","use_rate")]
  names(max_usage)[names(max_usage)=="use_rate"] <- "maximum_usage"
  
  if (any(target_usage<=max(country_use_rate[country_use_rate$iso3 %in% country_iso3,"use_rate"]))==FALSE) {
    print(max_usage)
    stop("Target usage(s) cannot be achieved with estimated use rate in any of the countries. Refer to maximum usage above.")
  } else if(any(target_usage>min(country_use_rate[country_use_rate$iso3 %in% country_iso3,"use_rate"]))) {
    print(max_usage)
    print("Target usage(s) cannot be achieved with estimated use rate in some of the countries - return NA. Refer to maximum usage above.")
  }

  targets_all <- lapply(target_usage, function(this_use) {
    targets_dt <- country_use_rate[country_use_rate$iso3 %in% country_iso3, 
                                   c("iso3", "year", "use_rate")]
    targets_dt$target_use <- this_use
    # Convert target usage to target access: access = usage/use rate
    # since use rate = the proportion of people with access to a net who slept under it
    targets_dt$target_access <- targets_dt$target_use/targets_dt$use_rate
    targets_dt <- targets_dt[order(targets_dt$target_access),]
    targets_dt$target_percapita_nets <- NA
    targets_dt$target_percapita_nets[
      targets_dt$target_access<=max(loess_for_prediction$loess_predicted_access)] <- 
      approx(loess_for_prediction$loess_predicted_access,
             loess_for_prediction$percapita_nets, 
             targets_dt$target_access[targets_dt$target_access<=max(loess_for_prediction$loess_predicted_access)])$y
    # Relationship between access and NPC is aggregated for all countries,
    # but use rate to convert usage to access is country-specific (derived from spatiotemporal regression?)
    return(targets_dt[order(targets_dt$iso3),])
  })
  targets_all <- do.call("rbind", targets_all)
  return(targets_all)
  
 }

x <- find_npc(target_usage=seq(0.1,0.9,0.1))

####


### TO DO
# Find updated use rate data for 2020
# Convert nets per capita to net crop using pop. at risk?
# Any further extrapolation options?
