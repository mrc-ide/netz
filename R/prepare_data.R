#' Function for reading in and formatting required datasets
#'
#' @return 3 datasets of use rate by country, Loess curve of access against nets 
#' per capita, and estimated median net retention times by country in days
#' 
#' @importFrom utils 'read.csv'
#' @export

prepare_data <- function() {

  ### Inputs ###

  cube_nat_level_annual <- read.csv("https://raw.github.com/bertozzivill/map-itn-gts/master/data/coverage_metrics/aggregated_predictions_2019.csv")
  cube_nat_level_annual  <- cube_nat_level_annual[cube_nat_level_annual$iso3 != "AFR" &
                                                    cube_nat_level_annual$year == 2019,]
  # 2019 data of access, use rate and npc
  
  cube_nat_level <- read.csv("https://raw.github.com/bertozzivill/map-itn-cube/publication-2021/paper_figures/figure_data/fig_4_access_npc.csv")
  # 2020 data of access, use rate and npc
  
  half_life_data <- read.csv("https://raw.github.com/bertozzivill/map-itn-cube/publication-2021/paper_figures/figure_data/fig_5_llin_half_lives.csv")[,c("iso3", "half_life")]
  # 2020 estimates of net half lives (median retention times)  in years
  half_life_data$half_life <- half_life_data$half_life*365
  
  ### Data Prep ###
  
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
  
  return(list(use_rate_by_country = cube_nat_level_annual[,c("iso3", "year", "use_rate")],
              loess_for_prediction = loess_for_prediction,
              half_life_data = half_life_data))
  
}
