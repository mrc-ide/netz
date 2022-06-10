#' Reading in and formatting half life datasets
#'
#' @param path Link to dataset.
#'
#' @export
#' @return Half life dataset
get_halflife_data <- function(path = paste0("https://raw.github.com/",
                                            "bertozzivill/map-itn-cube/",
                                            "publication-2021/paper_figures/",
                                            "figure_data/fig_5_llin_half_lives.csv")){
  # estimates of net half lives (median retention times) in years
  halflife_data <- utils::read.csv(path)
  
  # Convert half lives into days
  halflife_data <- halflife_data[, c("iso3", "half_life")]
  halflife_data$half_life <- halflife_data$half_life * 365
  
  return(halflife_data)
}

#' Function for reading in and formatting usage rate datasets
#'
#' @param path Link to dataset.
#' 
#' @export
#' @return Dataset of use rate by country
get_usage_rate_data <- function(path = paste0("https://raw.github.com/",
                                             "bertozzivill/map-itn-gts/",
                                             "master/data/coverage_metrics/",
                                             "aggregated_predictions_2019.csv")){
  # estimates of net half lives (median retention times) in years
  usage_rate_data <- utils::read.csv(path)
  
  # Convert half lives into days
  usage_rate_data <- usage_rate_data[usage_rate_data$variable == "use_rate", ]
  usage_rate_data <- usage_rate_data[!usage_rate_data$iso3 == "AFR",]
  usage_rate_data <- usage_rate_data[is.na(usage_rate_data$month),]
  usage_rate_data <- usage_rate_data[, c("iso3", "mean")]
  colnames(usage_rate_data) <- c("iso3", "usage_rate")

  return(usage_rate_data)
}

#' Function for reading in and access and npc data
#'
#' @param path Link to dataset.
#' 
#' @export
#' @return Dataset of use rate by country
get_npc_data <- function(path = paste0("https://raw.github.com/",
                                       "bertozzivill/map-itn-cube/",
                                       "publication-2021/paper_figures/",
                                       "figure_data/fig_4_access_npc.csv")){
  utils::read.csv(path)
}
