#' Function of net loss over time.
#' 
#' Net loss follows an S-shaped smooth compact curve, from Bertozzi-Villa, Amelia, et al. Nature communications 12.1 (2021): 1-12.
#' @param t Single value or vector of timesteps in days.
#' @param k Fixed rate. Default = 20 from Bertozzi-Villa, Amelia, et al. Nature communications 12.1 (2021): 1-12.
#' @param half_life (Country-specific) half-life of nets in days.
#'
#' @return The proportion of nets retained over time.
#' @export
net_loss_map <- function(t, half_life, k = 20) {
  if(half_life <= 0){
    stop("half_life must be > 0")
  }
  # Convert half life into the time at which no nets are retained (nets=0) in days:
  l <- half_life / sqrt(1 - k / (k - log(0.5)))

  prop_retained <- exp(k - k / (1 - (t / l)^2))
  prop_retained[t >= l] <- 0
  return(prop_retained)
}

#' Function of net loss over time from malariasimulation
#' Net loss is exponential.
#' @param t Single value or vector of timesteps in days.
#' @param mean_retention (Country-specific) average duration of net retention in days.
#'
#' @return The proportion of nets retained over time.
#' @export
net_loss_exp <- function(t, mean_retention) {
  if(mean_retention <= 0){
    stop("mean_retention must be > 0")
  }
  loss_rate <- 1 / mean_retention
  prop_retained <- exp(-loss_rate * t)
  return(prop_retained)
}
