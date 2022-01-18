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

### Original approach ----

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

### Option 1: increase use rate to 1 to reach the desired usage

# One option I considered was to, in these cases, set the use rate to increase to 1 or 
# the highest observed level anywhere (0.96) to reach the desired usage. 
# That would keep all target access values close to, but below, 1. It means
# however that in a few countries, the target access and hence nets per capita 
# will not increase with increasing coverage (e.g., nets per capita for target 
# usage of 80% would be less than those for 70%), so that doesn’t really make sense.

target_uses <- seq(0.5, 0.9, 0.1)

# Firstly, change dataset so as to extrapolate linearly between last access point and 1
# on the access-NPC curve
loess_for_prediction <- subset(loess_for_prediction, 
                               !(percapita_nets %in% npc_to_extrapolate))
loess_for_prediction <- rbind(loess_for_prediction,
                              data.frame(iso3=NA, month=NA, access=1, 
                                         percapita_nets = 1, loess=1))

targets_all <- lapply(target_uses, function(this_use){
  targets_dt <- cube_nat_level_annual[, list(iso3, year, access, use, use_rate, 
                                             percapita_nets, target_use=this_use)]
  # Convert target usage to target access: access = usage/use rate
  # since use rate = the proportion of people with access to a net who slept under it
  targets_dt[, new_use_rate := ifelse(use_rate>=target_use,use_rate,1)]
  targets_dt[, target_access:= target_use/new_use_rate]
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

ggplot(targets_all) +
  geom_line(aes(x=target_use, y = target_access, group = iso3))

### Option 2: limit  access to 1 where it exceeds it (or increase use rate to target usage where it is lower) ----

# Another simple option would be to increase the use rate to the target usage 
# (where it is below this value), giving a target access of 1. This means that 
# increasing target usage will lead to increasing or constant target access. 
# However, it also means that in some countries, a target access of 1 is already 
# reached with a 70% target usage. That would imply that reaching a target usage 
# of 70%, 80%, 90% etc. in these countries would all require the same nets per capita 
# and hence presumably the same cost. The assumption behind this being that any 
# further increases in usage are not achieved through distribution of more bednets, 
# but through making it more efficient (at no extra cost). 

targets_all <- lapply(target_uses, function(this_use){
  targets_dt <- cube_nat_level_annual[, list(iso3, year, access, use, use_rate, 
                                             percapita_nets, target_use=this_use)]
  # Convert target usage to target access: access = usage/use rate
  # since use rate = the proportion of people with access to a net who slept under it
  targets_dt[, target_access:= ifelse(target_use/use_rate<=1, target_use/use_rate, 1)]
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
targets_all$new_use_rate <- targets_all$target_use/targets_all$target_access
# This approach is the same as setting the use_rate to target_use

ggplot(targets_all) +
  geom_line(aes(x=target_use, y = target_access, group = iso3))

ggplot(targets_all) +
  geom_line(aes(x=target_use, y = target_percapita_nets, group = iso3))

### To work on:

target_uses <- seq(0.6, 0.9, 0.1)
targets_all <- lapply(target_uses, function(this_use){
  targets_dt <- cube_nat_level_annual[, list(iso3, year, access, use, use_rate, 
                                             percapita_nets, target_use=this_use)]
  # Convert target usage to target access: access = usage/use rate
  # since use rate = the proportion of people with access to a net who slept under it
  targets_dt[, use_rate2:= ifelse(use_rate<target_use, 
                                  approx(x=c(use,1), c(use_rate,1), xout=c(target_use),
                                         rule=2, method="linear")$y, 
                                  use_rate)] # TODO: what if target_access > 1?
  targets_dt[, target_access:= target_use/use_rate2] # TODO: what if target_access > 1?
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

# Setting use rate to maximum desired target use instead! (max(target_uses))

ggplot(targets_all) +
  geom_line(aes(x=target_use, y = target_access, group = iso3))

ggplot(targets_all) +
  geom_line(aes(x=target_use, y = target_percapita_nets, group = iso3))

# How does use rate increase with usage?

ggplot() +
  geom_point(data=cube_nat_level_annual,
             aes(x=use, y = use_rate)) + ylim(c(0,1)) + xlim(c(0,1)) +
  geom_line(aes(x=sort(cube_nat_level_annual$use), 
                y=predict(lm_model, data.frame(use=sort(cube_nat_level_annual$use))))) +
  geom_line(aes(x=seq(0,1, 0.05), y = seq(0,1, 0.05)*slope+intercept2), col = "red")
# 0.9608982, use = 0.4518110

usex <- 0.4518110
slope <- 0.3284
intercept1 <- 0.6856
intercept2 <- intercept1+(cube_nat_level_annual$use_rate[round(cube_nat_level_annual$use,6)==usex]-
                            (slope*usex+intercept1))
# slope is 1/access

# Options would be to assume linear increase in use rate with usage from country-
# specific coverage to 1 (assuming different relationship between usage and use rate in each 
# country)
# or assume some underlying relationship between use rate and usage and apply this 
# slope to all points

# Second option:
target_uses <- seq(0.6, 0.9, 0.1)
targets_all <- lapply(target_uses, function(this_use){
  targets_dt <- cube_nat_level_annual[, list(iso3, year, access, use, use_rate, 
                                             percapita_nets, target_use=this_use)]
  # Convert target usage to target access: access = usage/use rate
  # since use rate = the proportion of people with access to a net who slept under it
  targets_dt[, use_rate2:= target_use*slope+intercept1+(use_rate-(slope*use+intercept1))] 
  targets_dt[, use_rate2:= ifelse(use_rate2>1,1,use_rate2)]
  targets_dt[, target_access:= target_use/use_rate2] # TODO: what if target_access > 1?
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

lm_model <- lm(use_rate ~ use, data=cube_nat_level_annual)
predict(lm_model, data.frame(use=sort(cube_nat_level_annual$use)))

# plot use vs nets per capita
#x = use, y = use rate
approx(x=c(0.8047311,1), c(0.9382432,1), xout=c(0.9),
       rule=2, method="linear")$y


### Next steps ----

# Next, convert nets per capita to net crop using pop. at risk?
# To do:
# Turn into a function that takes as input the ISO and target usage, then returns NPC
# Could input also be subnational?
