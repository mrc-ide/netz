#' Function for reading in and formatting required datasets
#' 
#' @param aggregated_annual_data_path Link to dataset containing annual net use rate by country and year.
#' Must have columns with the following names: iso3, year, month, mean, variable.
#' @param aggregated_annual_data_year Single year for which to select net use rate. Must be contained in
#' the dataset in aggregated_annual_data_path.  
#' @param access_npc_monthly_data_path Link to dataset containing net access and nets per capita by country, year and month.
#' Must have columns with the following names: iso3, year, month, access_mean, percapita_nets_mean.
#' @param half_life_data_path Link to dataset containing net half lives (median retention times) in years by country.
#' Must have columns with the following names: iso3, half_life.
#'
#' @return 3 datasets of use rate by country, Loess curve of access against nets 
#' per capita, and estimated median net retention times by country in days
#' 
#' @importFrom utils 'read.csv'
#' @export

prepare_data <- function(aggregated_annual_data_path = "https://raw.github.com/bertozzivill/map-itn-gts/master/data/coverage_metrics/aggregated_predictions_2019.csv", 
                         aggregated_annual_data_year = 2019,
                         access_npc_monthly_data_path = "https://raw.github.com/bertozzivill/map-itn-cube/publication-2021/paper_figures/figure_data/fig_4_access_npc.csv",
                         half_life_data_path = "https://raw.github.com/bertozzivill/map-itn-cube/publication-2021/paper_figures/figure_data/fig_5_llin_half_lives.csv") {

  ### Inputs ###

  # data of use rate, access and npc in given year
  cube_nat_level_annual <- read.csv(aggregated_annual_data_path)

  cube_nat_level <- read.csv(access_npc_monthly_data_path)
  
  # estimates of net half lives (median retention times) in years
  half_life_data <- read.csv(half_life_data_path)
  
  ### Data format checks ###
  
  if (any(!c("iso3", "year", "month", "mean", "variable") %in% 
          colnames(cube_nat_level_annual))) {
    stop("Dataset in aggregated_annual_data_path must contain the following columns: iso3, year, month, mean, variable")
  }
  if (!(aggregated_annual_data_year %in% cube_nat_level_annual$year)) {
    stop("aggregated_annual_data_year does not correspond to year in dataset in aggregated_annual_data_path")
  }
  if (!("use_rate" %in% cube_nat_level_annual$variable)) {
    stop("Dataset in aggregated_annual_data_path must contain use_rate in the variable column")
  }
  if (any(!c("iso3", "year", "month", "access_mean", "percapita_nets_mean") %in% 
          colnames(cube_nat_level))) {
    stop("Dataset in access_npc_monthly_data_path must contain the following columns: iso3, year, month, access_mean, percapita_nets_mean")
  }
  if (any(!c("iso3", "half_life") %in% 
          colnames(half_life_data))) {
    stop("Dataset in half_life_data_path must contain the following columns: iso3, half_life")
  }
  
  ### Data Prep ###
  
  # Convert half lives into days 
  half_life_data <- half_life_data[,c("iso3", "half_life")]
  half_life_data$half_life <- half_life_data$half_life*365
  
  # Aggregate annual data
  # From this, annual mean use rate is later used to convert target usage to target access
  cube_nat_level_annual  <- cube_nat_level_annual[cube_nat_level_annual$iso3 != "AFR" &
                                                    cube_nat_level_annual$year == aggregated_annual_data_year,]
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
