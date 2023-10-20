#' Estimate average population usage given at equilibrium
#'
#'
#' @param distribution The % of the population who are distributed a net
#' @param distribution_frequency The frequency of the distribution cycle (days)
#' @param mean_retention The average duration of net retention (days)
#'
#' @return The equilibrium population-level net usage
#' @export
exp_loss_equilibrium <- function(distribution, distribution_frequency, mean_retention){
  distribution * mean_retention / distribution_frequency * (1 - exp(-distribution_frequency / mean_retention))
}

#' Estimate the population-level bed net usage at given times for a set of
#' bed net distributions at given times. This assumes a constant rate of net loss
#' (as in malariasimulation) and that recipients of multiple rounds are random.
#' 
#' @param timesteps The half life of the exponentially decaying net loss function (days)
#' @param distribution A vector of distribution, must be between 0 and 1
#' @param distribution_timesteps A vector of time steps for distributions (days)
#' @param mean_retention The average duration of net retention (days)
#'
#' @return Usage estimates at timepoints
#' @export
population_usage_t <- function(
    timesteps,
    distribution,
    distribution_timesteps,
    mean_retention = 365 * 5
){
  loss_rate <- 1 / mean_retention
  
  # Estimate the cumulative usage at distribution time points
  cumulative_usage <- distribution[1]
  if(length(distribution_timesteps) > 1){
    for(t in 2:length(distribution_timesteps)){
      time_offset <- distribution_timesteps[t] - distribution_timesteps[t - 1]
      remaining <- cumulative_usage[t - 1] * exp(-loss_rate * time_offset)
      cumulative_usage[t] <- 1 - (1 - remaining) * (1 - distribution[t])
    }
  }
  
  # Estimate the usage at target time points
  usage <- c()
  for(t in seq_along(timesteps)){
    time_offset <- timesteps[t] - distribution_timesteps
    if(max(time_offset) < 0){
      usage[t] <- 0
    } else {
      nearest <- min(time_offset[time_offset >= 0])
      index <- which(time_offset == nearest)
      usage[t] <- cumulative_usage[index] * exp(-loss_rate * time_offset[index])
    }
  }
  return(usage)
}

