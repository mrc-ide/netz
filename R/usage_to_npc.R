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

rm(list=ls())

library(data.table)
library(ggplot2)
library(pracma)

############ ----------------------------------------------------------------------------------------------------------------------
## Inputs  ----------------------------------------------------------------------------------------------------------------------
############ ----------------------------------------------------------------------------------------------------------------------

analysis_year <- 2019
cube_indir <- "https://raw.github.com/bertozzivill/map-itn-gts/master/data/coverage_metrics/aggregated_predictions_2019.csv"

write_results <- F

############ ----------------------------------------------------------------------------------------------------------------------
## Data Prep  ---------------------------------------------------------------------------------------------------------
############ ----------------------------------------------------------------------------------------------------------------------

# TODO: Do we need to convert to population-at-risk?

cube_nat_level <- read.csv(cube_indir)
cube_nat_level <- subset(cube_nat_level, iso3!="AFR" &  year %in% analysis_year)
# Only covers Africa

cube_nat_level_annual <- cube_nat_level[is.na(cube_nat_level$month),]
cube_nat_level_annual <- dcast.data.table(data.table(cube_nat_level_annual), 
                                           iso3 + year ~ variable,
                                           value.var="mean")

cube_nat_level <- cube_nat_level[!is.na(cube_nat_level$time),]
# cube_nat_level contains data by month and cube_nat_level_annual for the whole year 
# (aggregated)
# From this, annual mean use rate is used to convert target usage to target access

# Monthly mean is used to generate loess curve of access vs NPC
cube_nat_level <- dcast.data.table(data.table(cube_nat_level), iso3 + month ~ variable, 
                                   value.var="mean")
# curve_fit <- loess(access ~ percapita_nets, data=cube_nat_level)
curve_fit <- loess(access ~ percapita_nets, data=cube_nat_level) 

# Generate a curve of access against NPC based on data from all countries and all months
# so we can later interpolate values for any access
loess_for_prediction <- cube_nat_level[, list(iso3, month, access, percapita_nets, loess=predict(curve_fit))]
loess_for_prediction <- loess_for_prediction[order(loess)]

# Option to extrapolate access for higher NPC which are not in the dataset
# npc_to_extrapolate <- seq(round(max(cube_nat_level$percapita_nets),2),
#                           1, by = 0.01)
# loess_for_prediction <- rbind(cube_nat_level[,list(iso3, month, access, percapita_nets)],
#                               data.frame(iso3=NA, month=NA, access=NA, 
#                                          percapita_nets = npc_to_extrapolate))
# loess_for_prediction$loess <- predict(curve_fit,loess_for_prediction$percapita_nets)
# loess_for_prediction <- loess_for_prediction[order(loess)]  # need to be ordered for interpolation
# loess contains predicted access for a given NPC

#rm(cube_nat_level_fnames, cube_nat_level, curve_fit)

# Adapt dataset so as to extrapolate linearly between last access point and 1
# on the access-NPC curve
# loess_for_prediction <- subset(loess_for_prediction, 
#                                !(percapita_nets %in% npc_to_extrapolate))
loess_for_prediction <- rbind(loess_for_prediction,
                              data.frame(iso3=NA, month=NA, access=1, 
                                         percapita_nets = 1, loess=1))

# Plot curve of access against nets per capita
ggplot(loess_for_prediction) +
  geom_point(aes(x=percapita_nets, y = access))+
  geom_line(aes(x=percapita_nets, y = loess), col = "turquoise", size=2) +
    ylab("Access") + xlab("Nets per capita") +
  ylim(0,1) +xlim(0,1)

# For this specific dataset, the loess_for_prediction output is always the same 
# so could be saved

############ ----------------------------------------------------------------------------------------------------------------------
## Find new NPC  ---------------------------------------------------------------------------------------------------------
############ ----------------------------------------------------------------------------------------------------------------------


# Input usage, convert this to access, then use Loess curve to generate corresponding NPC
target_uses <- seq(0.1, 1, 0.05)

targets_all <- lapply(target_uses, function(this_use){
  targets_dt <- cube_nat_level_annual[, list(iso3, year, access, use, use_rate, 
                                             percapita_nets, target_use=this_use)]
# Convert target usage to target access: access = usage/use rate
# since use rate = the proportion of people with access to a net who slept under it
  targets_dt[, target_access:= target_use/use_rate] # TODO: what if target_access > 1?
  targets_dt <- targets_dt[order(target_access)]
  targets_dt[target_access<=max(loess_for_prediction$loess), 
             target_percapita_nets:= interp1(loess_for_prediction$loess, 
                                             loess_for_prediction$percapita_nets, 
                                             target_access)] 
  # Relationship between access and NPC is aggregated for all countries,
  # but use rate to convert usage to access is country-specific (derived from spatiotemporal regression?)
  return(targets_dt[order(iso3)])
})

targets_all <- rbindlist(targets_all)
#targets_all[, target_use:=factor(target_use)]

# As function: (replace datasets)
find_npc <- function(target_usage, country_iso3) {

  if (any(target_usage<cube_nat_level_annual[iso3 %in% country_iso3,use_rate])==FALSE) {
    stop("Target usage(s) cannot be achieved with estimated use rate in any of the countries.")
    
  } else {
    
    #if (any(cube_nat_level_annual[iso3 %in% country_iso3,use_rate]<target_usage)) {
      max_usage <- cube_nat_level_annual[iso3 %in% country_iso3,list(iso3,use_rate)]
      for (i in 1:length(target_usage)) {
        countries_exceeded <- max_usage$iso3[max_usage$use_rate<target_usage[i]]
        if(length(countries_exceeded)>=1) {
          print(paste0("Target usage of ", target_usage[i], 
                       " cannot be achieved with estimated use rate in ", 
                       toString(countries_exceeded),
                       " - return NA"))
        }
      }
    #}
     
    targets_all <- lapply(target_usage, function(this_use){
      targets_dt <- cube_nat_level_annual[iso3 %in% country_iso3, list(iso3, year, access, use, use_rate, 
                                                                       percapita_nets, target_use=this_use)]
      # Convert target usage to target access: access = usage/use rate
      # since use rate = the proportion of people with access to a net who slept under it
      targets_dt[, target_access:= target_use/use_rate] # TODO: what if target_access > 1?
      targets_dt <- targets_dt[order(target_access)]
      targets_dt[target_access<=1, #target_access<=max(loess_for_prediction$loess)
                 target_percapita_nets:= interp1(c(loess_for_prediction$loess,1), 
                                                 c(loess_for_prediction$percapita_nets,1), 
                                                 target_access)] 
      #targets_dt <- targets_dt[targets_dt$target_access<=1,]
      # Relationship between access and NPC is aggregated for all countries,
      # but use rate to convert usage to access is country-specific (derived from spatiotemporal regression?)
      return(targets_dt[order(iso3)])
    })
    targets_all <- rbindlist(targets_all)
    return(targets_all)
  }
  
}

x <- find_npc(c(0.7,0.8), cube_nat_level_annual$iso3)


### Next steps ----

# Find updated data
# Next, convert nets per capita to net crop using pop. at risk?
# To do:
# Turn into a function that takes as input the ISO and target usage, then returns NPC
# Could input also be subnational?
