#' Convert bednet usage to annual nets distributed per capita
#' 
#' Assumptions: 
#' - Net use rates stays at 2019 levels for all countries
#' - When converting from access to NPC, use the 2020 loess curve fit of access-NPC 
#'   at the country-month level.
#' - When determining the number of nets to distribute in each mass campaign, 
#'   assume mass campaigns occur every three years.
#' 
#' @param target_usage A single value or vector of desired target usages to model.
#' @param country_iso3 Vector of ISO3 codes of African countries for which to return results. 
#' Default = "all".
#' @param extrapolate_npc Option to extrapolate target nets per capita beyond fitted 
#' Loess curve. Default = "linear" for linear extrapolation. For any other inputs,
#' NA outputs are returned for any target usages exceeding currently observed levels.
#' @param k Fixed rate parameter of the internal function of net loss, used to 
#' estimate the median net retention times by country. Default = 20 from
#' Bertozzi-Villa, Amelia, et al. Nature communications 12.1 (2021): 1-12.
#'
#' @return Dataframe of access, nets per capita and annual nets distributed per capita
#' for the desired steady-state target usage by country
#' 
#' @importFrom stats 'approx' 'reshape' 'loess' 'predict'
#' @export

find_annual_nets_distributed <- function(target_usage, country_iso3="all", 
                                         extrapolate_npc = "linear", k=20) {
  
  ### 1. Convert target usage to nets per capita ###
  
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
  targets_all$target_access[targets_all$target_access>1] <- NA
  # Set access to NA where it is over 1 and excluded from conversion
  
  ### 2. Convert equilibrium nets per capita to annual nets distributed per capita ###
  
  # Read in net half lives (median retention times) for each country
  half_life_data <- data$half_life_data
  
  # Estimate time at which all nets are lost (parameter l) based on net half life in each country
  half_life_data$l <- half_life_data$half_life/sqrt(1 - k / (k - log(0.5)))
  
  # Merge this with target nets per capita by country and target usage
  nets_distributed <- merge(x=targets_all, y=half_life_data[,c("iso3", "l")], 
                            by = "iso3", all.x=TRUE, all.y=FALSE)
  
  # Function of net loss returns the proportion of nets retained over time
  # k is a fixed rate from paper (k=20)
  # l = country-specific time at which all nets = 0
  # Half life corresponds to prop_retained = 0.5
  net_loss <- function(t, k, l) {
    prop_retained <- exp(k - k / (1 - (t / l) ^ 2))
    prop_retained[t >= l] <- 0
    return(prop_retained)
  }
  
  # Function to integrate net loss function over 3 year period
  # and function to vectorise this for application on multiple half-lives
  integrate_net_loss <- function(k,l) {
    stats::integrate(net_loss, lower = 0, upper = 3, k=k, l =l)$value
  }
  integrate_net_loss_vector <- Vectorize(integrate_net_loss)
  
  # Use this to calculate annual nets distributed per capita for different countries 
  # and target usages
  nets_distributed$annual_percapita_nets_distributed <- nets_distributed$target_percapita_nets * 
    integrate_net_loss_vector(k=k, l = nets_distributed$l) * (1/3)
  nets_distributed <- nets_distributed[,c("iso3", "use_rate", "target_use",
                                          "target_access", "target_percapita_nets",
                                          "annual_percapita_nets_distributed")]
  
  return(nets_distributed[order(nets_distributed$iso3, nets_distributed$target_use),])
  
}

