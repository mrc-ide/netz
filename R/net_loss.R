#' Function of net loss over time from Bertozzi-Villa, Amelia, et al. Nature communications 12.1 (2021): 1-12.
#' Net loss follows an S-shaped “smooth compact” curve.
#' Used to estimate the median net retention times by country.
#' @param t Single value or vector of timesteps in days.
#' @param k Fixed rate. Default = 20 from Bertozzi-Villa, Amelia, et al. Nature communications 12.1 (2021): 1-12.
#' @param half_life (Country-specific) half-life of nets in days.
#' 
#' @return The proportion of nets retained over time.
#' @export
net_loss_map <- function(t, k, half_life, ...) {
  
  # Convert half life into the time at which no nets are retained (nets=0) in days:
  l <- half_life/sqrt(1 - k / (k - log(0.5)))

  prop_retained <- exp(k - k / (1 - (t / l) ^ 2))
  prop_retained[t >= l] <- 0
  return(prop_retained)
}

#' Function of net loss over time from malariasimulation
#' Net loss is exponential.
#' @param t Single value or vector of timesteps in days.
#' @param half_life (Country-specific) half-life of nets in days.
#' 
#' @return The proportion of nets retained over time.
#' @export
net_loss_exp <- function(t, half_life, ...){
  prop_retained <- exp(-(1 / half_life) * t)
  return(prop_retained)
}




