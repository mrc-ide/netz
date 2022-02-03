#' Convert annual nets distributed per capita to bednet usage
#' 
#' Assumptions: 
#' - Net use rates stays at 2019 levels for all countries
#' - When converting from NPC to access, use the 2020 loess curve fit of access-NPC 
#'   at the country-month level.
#' 
#' @param annual_percapita_nets_distributed A single value or vector of nets distributed per capita to model.
#' @param country_iso3 Vector of ISO3 codes of African countries for which to return results. 
#' Default = "all".
#' @param extrapolate_npc Option to extrapolate target nets per capita beyond fitted 
#' Loess curve. Default = "linear" for linear extrapolation. For any other inputs,
#' NA outputs are returned for any target usages exceeding currently observed levels.
#' @param k Fixed rate parameter of the internal function of net loss, used to 
#' estimate the median net retention times by country. Default = 20 from
#' Bertozzi-Villa, Amelia, et al. Nature communications 12.1 (2021): 1-12.
#'
#' @return Dataframe of access, nets per capita and net usage for the desired 
#' steady-state annual nets distributed per capita by country.
#' 
#' @importFrom stats 'approx' 'reshape' 'loess' 'predict'
#' @export
find_usage <- function(annual_percapita_nets_distributed, country_iso3="all", 
                       extrapolate_npc = "linear", k=20) {
  
  # Read in datasets of country-specific use rate and the Loess curve of 
  # access vs nets per capita
  data <- prepare_data()
  country_use_rate <- data$use_rate_by_country
  loess_for_prediction <- data$loess_for_prediction
  
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
  
  ### 1. Convert annual nets distributed per capita to equilibrium nets per capita ###
  
  # Create dataframe of all combinations of input countries and nets distributed
  nets_distributed <- expand.grid(iso3 = country_iso3,
                                  annual_percapita_nets_distributed = annual_percapita_nets_distributed)
  
  # Net half lives (median retention times) for each country
  half_life_data <- data$half_life_data
  
  # Estimate time at which all nets are lost (parameter l) based on net half life in each country
  half_life_data$l <- half_life_data$half_life/sqrt(1 - k / (k - log(0.5)))
  
  # Merge this with annual net distribution by country 
  nets_distributed <- merge(x=nets_distributed, y=half_life_data[,c("iso3", "l")], 
                            by = "iso3", all.x=TRUE, all.y=FALSE)
  
  # Use the net loss function to calculate the equilibrium nets per capita for the given
  # annual nets distribution per capita by country
  # This is for the specific "smoothed" case where we assume that 1/3 of the population at risk
  # is distributed nets every 3 years (this is equivalent to a smoothed 3 yearly whole 
  # population distribution).
  nets_distributed$target_percapita_nets <- nets_distributed$annual_percapita_nets_distributed/ 
    (integrate_net_loss_vector(k=k, l = nets_distributed$l) * (1/3))
  
  ### 2. Convert nets per capita to target usage ###
  
  # Use Loess curve to convert nets per capita to access
  nets_distributed$target_access <- 
    approx(x=loess_for_prediction$percapita_nets, 
           y=loess_for_prediction$loess_predicted_access,
           nets_distributed$target_percapita_nets)$y
  
  # Convert target access to target usage: usage = access * use rate
  # since use rate = the proportion of people with access to a net who slept under it
  # Relationship between access and NPC is aggregated for all countries,
  # but use rate to convert usage to access is country-specific (derived from spatiotemporal regression?)
  nets_distributed <- merge(x=nets_distributed, y=country_use_rate, 
                            by = "iso3", all.x=TRUE, all.y=FALSE)
  nets_distributed$target_usage <- nets_distributed$target_access*nets_distributed$use_rate
  
  nets_distributed <- nets_distributed[order(nets_distributed$iso3, nets_distributed$annual_percapita_nets_distributed),]
  
  return(nets_distributed[,c("iso3", "use_rate", "annual_percapita_nets_distributed",
                             "target_percapita_nets","target_access", 
                             "target_usage")])
  
  # TO DO:
  # May need to extrapolate on the lower end of the curve - currently returning NAs
  # What to do if percapita nets >1? Set to NA?
  
}
