#' Estimate average population usage given at equilibrium
#'
#'
#' @param distribution The % of the population who are distributed a net
#' @param distribution_frequency The frequency of the distribution cycle (years)
#' @param net_loss_hl The half life of the exponentially decaying net loss function (years)
#'
#' @return The equilibrium population-level net usage
#' @export
exp_loss_equilibrium <- function(distribution, distribution_frequency, net_loss_hl){
  distribution * net_loss_hl / distribution_frequency * (1 - exp(-distribution_frequency / net_loss_hl))
}

#' Sample those to receive an intervention with correlation
#' 
#' Code adapted from malariasimulation
#'
#' @param p Probability of receiving a bed net
#' @param mvnorm A vector of multivariate normal (correlated) draws.
#' @param sigma_squared Sigma squared
#' 
#'
#' @return A boolean vector denoting which individuals will receive the intervention
sample_intervention <- function(p, mvnorm, sigma_squared) {
  sd <- sqrt(1 + sigma_squared)
  u0 <- -stats::qnorm(p, 0) * sd
  z <- stats::rnorm(length(mvnorm))
  u0 + mvnorm + z < 0
}

#' Get correlation inputs for intervention sampling
#' 
#' Code adapted from malariasimulation
#' 
#' @param population Population size
#' @param rho Correlation parameter
#' 
#'
#' @return A names list with the multivariate random normal draw and sigma squared
get_correlation <- function(population, rho){
  if(rho <0 | rho >=1){
    stop("rho for bednets must be between 0 and 1")
  }
  sigma_squared <- rho / (1 - rho)
  V <- matrix(sigma_squared)
  mvnorm <- MASS::mvrnorm(population, 0, V)
  out <- list(mvnorm = mvnorm, 
              sigma_squared = sigma_squared)
  return(out)
}

#' Estimate the time series of population-level bed net usage from a vector of
#' bed net distributions.
#' 
#' Note: This is stochastic output
#'
#' @param net_loss A vector of the probability of still having a net over time
#' @param population Population size
#' @param timesteps Timesteps
#' @param distribution A vector of distribution %s
#' @param distribution_timesteps A vector of time steps for distributions
#' @param rho The between-round correlation parameter
#'
#' @return Usage time series vector
#' @export
population_usage <- function(
  net_loss,
  population,
  timesteps,
  distribution,
  distribution_timesteps,
  rho = 0){
  
  correlation <- get_correlation(population, rho)
  days_since_net <- rep(NA, population)
  usage <- rep(0, timesteps)
  d <- 1
  for(t in 1:timesteps){
    # Distribute nets
    if(t == distribution_timesteps[d] & d<= length(distribution_timesteps)){
      days_since_net[sample_intervention(distribution[d],correlation$mvnorm, correlation$sigma_squared)] <- 0
      d <- d + 1
    }
    # Estimate average usage
    nets <- net_loss[days_since_net + 1]
    nets[is.na(nets)] <- 0
    usage[t] <- mean(nets)
    # Increment age of nets
    days_since_net <- days_since_net + 1
  }
  return(usage)
}