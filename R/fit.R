#' Internal objective function for least-squares non-linear fitting of target usage
#'
#' @param distribution A vector of proposed distribution
#' @inheritParams fit_usage
#'
#' @return Sum of squared difference between proposed usage and target usage
fit_usage_objective <- function(distribution, distribution_timesteps,
                                target_usage, target_usage_timesteps,
                                population, net_loss, timesteps, rho,
                                seed){
  set.seed(seed)
  pu <- population_usage(
    net_loss = net_loss,
    population = population,
    timesteps = timesteps,
    distribution = distribution,
    distribution_timesteps =  distribution_timesteps,
    rho = rho)
  
  sum((pu[target_usage_timesteps] - target_usage) ^ 2)
}

#' Fit distribution to match target usage using non-linear optimisation
#'
#' @param target_usage Target usage
#' @param target_usage_timesteps Target usage time points
#' @param distribution_timesteps A vector of distribution time steps
#' @param distribution_lower Lower bound on distributions (default = 0)
#' @param distribution_upper Upper bound on distribution (default = 1)
#' @param population Population
#' @param net_loss Net loss vector
#' @param timesteps Timesteps
#' @param rho betwen round correlation parameters
#' @param seed Seed - the output of population_usage is stochastic.
#' @param ... Further arguments to pass to the \link[nloptr]{cobyla}
#' 
#' 
#' @return Non-linear optimisation fit output
#' @export
fit_usage <- function(target_usage, target_usage_timesteps,
                      distribution_timesteps,
                      distribution_init = rep(0, length(distribution_timesteps)),
                      distribution_lower = rep(0, length(distribution_timesteps)),
                      distribution_upper = rep(1, length(distribution_timesteps)),
                      population = 500,
                      timesteps = max(c(target_usage_timesteps, distribution_timesteps)),
                      net_loss = net_loss_exp(1:timesteps, half_life = 5 * 365),
                      rho = 0,
                      seed = 1234,
                      ...){
  
  if(any(distribution_lower < 0) | any(distribution_lower > 1)){
    stop("All distribution lower values must be between 0 and 1")
  }
  if(any(distribution_upper < 0) | any(distribution_upper > 1)){
    stop("All distribution upper values must be between 0 and 1")
  }
  if(length(distribution_lower) != length(distribution_timesteps) | length(distribution_upper) != length(distribution_timesteps)){
    stop("distribution_timesteps, distribution_lower and distribution_upper must have equal lengths")
  }
  if(length(target_usage) != length(target_usage_timesteps)){
    stop("Target usage and target usage timesteps must be the same length")
  }

  nloptr::cobyla(x0 = distribution_init,
                 fn = fit_usage_objective,
                 lower = distribution_lower,
                 upper = distribution_upper,
                 distribution_timesteps = distribution_timesteps,
                 target_usage = target_usage, 
                 target_usage_timesteps = target_usage_timesteps,
                 net_loss = net_loss,
                 population = population,
                 timesteps = timesteps,
                 rho = rho,
                 seed = seed,
                 ...)
  
}