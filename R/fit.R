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

#' Fit distibution to match target usage
#'
#' @param target_usage Target usage
#' @param target_usage_timesteps Target usage time points
#' @param distribution_timesteps A vector of distribution time steps
#' @param distribution_lower Lower bound on distributions (default = 0)
#' @param distribution_upper Upper bound on distribution (default = 1)
#' @param population Population
#' @param net_loss Net loss function
#' @param timesteps Timesteps
#' @param seed Seed - the output of population_usage is stochastic.
#' 
#' 
#' @return Non-linear optimisation fit output
#' @export
fit_usage <- function(target_usage, target_usage_timesteps,
                      distribution_timesteps,
                      distribution_lower = rep(0, length(distribution_timesteps)),
                      distribution_upper = rep(1, length(distribution_timesteps)),
                      population = 500,
                      timesteps = 365 * 10,
                      net_loss = exp(- (1 / (5 * 365)) * 1:timesteps), 
                      rho = 0,
                      seed = 1234){
  
  nloptr::cobyla(x0 = rep(0, length(distribution_timesteps)),
                 fn = fit_usage_objective,
                 lower = distribution_lower,
                 upper = distribution_upper,
                 distribution_timesteps = distribution_timesteps,
                 target_usage = target, 
                 target_usage_timesteps = target_tt,
                 net_loss = net_loss,
                 population = population,
                 timesteps = timesteps,
                 rho = rho,
                 seed = seed)
  
}