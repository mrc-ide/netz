# Prepare datasets

############ ----------------------------------------------------------------------------------------------------------------------
## Inputs  ----------------------------------------------------------------------------------------------------------------------
############ ----------------------------------------------------------------------------------------------------------------------

cube_nat_level_annual <- read.csv("https://raw.github.com/bertozzivill/map-itn-gts/master/data/coverage_metrics/aggregated_predictions_2019.csv")
cube_nat_level_annual  <- subset(cube_nat_level_annual, iso3!="AFR" &  year == 2019)
# 2019 data

cube_nat_level <- read.csv("https://raw.github.com/bertozzivill/map-itn-cube/publication-2021/paper_figures/figure_data/fig_4_access_npc.csv")
# 2020 data

############ ----------------------------------------------------------------------------------------------------------------------
## Data Prep  ---------------------------------------------------------------------------------------------------------
############ ----------------------------------------------------------------------------------------------------------------------

# Aggregate annual data
# From this, annual mean use rate is used to convert target usage to target access
cube_nat_level_annual <- cube_nat_level_annual[is.na(cube_nat_level_annual$month),]
cube_nat_level_annual <- reshape(cube_nat_level_annual[,c("iso3", "mean", "year", "variable")], 
                idvar = c("iso3", "year"), timevar = "variable", direction = "wide",
                varying=list(c(unique(cube_nat_level_annual$variable))))

# Data by month
# Monthly mean is used to generate loess curve of access against nets per capita
cube_nat_level <- cube_nat_level[,c("iso3", "year","month", "access_mean", "percapita_nets_mean")]
names(cube_nat_level)[names(cube_nat_level) %in% 
                        c("access_mean", "percapita_nets_mean")] <- c("access", "percapita_nets")

# Fit Loess curve of access against NPC based on data from all countries and all months
# so we can later interpolate values for any access
curve_fit <- loess(access ~ percapita_nets, data=cube_nat_level) 
loess_for_prediction <- cube_nat_level[,c("iso3", "month", "access", "percapita_nets")]
loess_for_prediction$loess_predicted_access <- predict(curve_fit)
loess_for_prediction <- loess_for_prediction[order(loess_for_prediction$loess_predicted_access),]

# Plot curve of access against nets per capita
library(ggplot2)
plot_curve <- ggplot(loess_for_prediction) +
  geom_point(aes(x=percapita_nets, y = access))+
  geom_line(aes(x=percapita_nets, y = loess_predicted_access), col = "turquoise", size=2) +
  ylab("Access") + xlab("Nets per capita") +
  ylim(0,1) +xlim(0,1)

# Save datasets:
#write.csv(cube_nat_level_annual[,c("iso3", "year", "use_rate")], 
#          "data/use_rate_by_country.csv", row.names=FALSE) 
#write.csv(loess_for_prediction, "data/access_vs_npc_loess.csv", row.names=FALSE) 
