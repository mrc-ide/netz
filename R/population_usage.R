#' Estimate average population usage given at equilibrium
#'
#'
#' @param distribution The % of the population who are distributed a net
#' @param distribution_frequency The frequency of the distribution cycle (days)
#' @param net_loss_hl The half life of the exponentially decaying net loss function (days)
#'
#' @return The equilibrium population-level net usage
#' @export
exp_loss_equilibrium <- function(distribution, distribution_frequency, net_loss_hl){
  distribution * net_loss_hl / distribution_frequency * (1 - exp(-distribution_frequency / net_loss_hl))
}

#' Estimate the time series of population-level bed net usage from a vector of
#' bed net distributions. This is fixed for an exponentially distributed net
#' retention, as implemented in malariasimulation
#' 
#' @param half_life The half life of the exponentially decaying net loss function (days)
#' @param timesteps Timesteps
#' @param distribution A vector of distribution %s
#' @param distribution_timesteps A vector of time steps for distributions
#'
#' @return Usage time series vector
#' @export
population_usage <- function(
  distribution,
  distribution_timesteps,
  timesteps,
  half_life){
  
  t <- 0:timesteps
  net_loss <- net_loss_exp(t = t, half_life = half_life)
  
  use <- rep(0, timesteps)
  for(i in seq_along(distribution)){
    cur_dist <- c(rep(0, distribution_timesteps[i] - 1), distribution[i] * net_loss)[1:timesteps]
    use <- use + (cur_dist * (1 - use[distribution_timesteps[i]]))
  }
  return(use)
}
