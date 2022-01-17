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
curve_fit <- loess(access ~ percapita_nets, data=cube_nat_level, 
                   control=loess.control(surface="direct")) # Added this control option to allow extrapolation of values >~0.8 NPC

# Generate a curve of access against NPC based on data from all countries and all months
# so we can later interpolate values for any access
# Additionally, need to extrapolate access for higher NPC which are not in the dataset
npc_to_extrapolate <- seq(round(max(cube_nat_level$percapita_nets),2),
                          1, by = 0.01)
loess_for_prediction <- rbind(cube_nat_level[,list(iso3, month, access, percapita_nets)],
                              data.frame(iso3=NA, month=NA, access=NA, 
                                         percapita_nets = npc_to_extrapolate))
loess_for_prediction$loess <- predict(curve_fit,loess_for_prediction$percapita_nets)
loess_for_prediction <- loess_for_prediction[order(loess)]  # need to be ordered for interpolation
# loess contains predicted access for a given NPC

#rm(cube_nat_level_fnames, cube_nat_level, curve_fit)

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
target_uses <- seq(0.1, 0.9, 0.1)

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
targets_all[, target_use:=factor(target_use)]

#######
this_use <- 0.9
input_iso <-cube_nat_level_annual$iso3

# Do this for just one country:
targets_dt <- cube_nat_level_annual[iso3 %in% input_iso, 
                                    list(iso3, year, access, use, use_rate, 
                                           percapita_nets, target_use=this_use)]
# Convert target usage to target access: access = usage/use rate
# since use rate = the proportion of people with access to a net who slept under it
#targets_dt[, target_access:= ifelse(target_use/use_rate<=max(loess_for_prediction$loess), 
#                                    target_use/use_rate, max(loess_for_prediction$loess))] 
targets_dt[, target_access:= ifelse(target_use/use_rate<=1, target_use/use_rate, 1)] 
targets_dt <- targets_dt[order(target_access)]
# Test: linearly extreapolate over observed access:

loess_for_prediction <- subset(loess_for_prediction, 
                               !(percapita_nets %in% npc_to_extrapolate))
loess_for_prediction <- rbind(loess_for_prediction,
                              data.frame(iso3=NA, month=NA, access=1, 
                                         percapita_nets = 1, loess=1))
targets_dt[target_access<=max(loess_for_prediction$loess), 
           target_percapita_nets:= interp1(loess_for_prediction$loess, 
                                           loess_for_prediction$percapita_nets, 
                                           target_access)] 
# Relationship between access and NPC is aggregated for all countries,
# but use rate to convert usage to access is country-specific (derived from spatiotemporal regression?)
return(targets_dt[order(iso3)])

### Test: linearly extreapolate over observed access:
loess_for_prediction <- subset(loess_for_prediction, 
                               !(percapita_nets %in% npc_to_extrapolate))
loess_for_prediction <- rbind(loess_for_prediction,
                              data.frame(iso3=NA, month=NA, access=1, 
                                         percapita_nets = 1, loess=1))
# The consequence of this approach is that in some countries, different usages (e.g. 80, 90, 100%)
# suddenly have the same cost. So the assumption would be that reaching high usages
# does not involve distribution of more bednets but improved efficiency in using them
# but is that related to the use_rate or an actual linear extrapolation?
#####

targets_plot <- ggplot(targets_all, aes(x=use, y=target_percapita_nets, color=target_use)) +
  geom_text(aes(label=iso3)) +
  theme_bw()

if (write_results){
  write.csv(targets_all, "../use_to_npc_targets.csv", row.names = F)
}

# Next, convert nets per capita to net crop using pop. at risk?

# To do:
# Turn into a function that takes as input the ISO and target usage, then returns NPC
# Could input also be subnational?
# What if target_access > 1? Could set target access to 1 if it exceeds 1
# or assume access cannot practicably exceed estimates observed so far 
# by setting to the maximum access in any country. The latter would 
# then assume a >100% use rate

# If limiting to 1, this assumes the use rate reduces if access reaches 1 
# however these points then exceed what is considered "feasible"/observed access 
# in the access-NPC curve - so for that one might assume it can theoretically go
# up to 1

# Need to ensure that cost cannot become less for higher usage!

# One feasible assumption may be to extrapolate curve to e.g. 90% access and
# above that assume higher usage is reached through improved efficiency.
