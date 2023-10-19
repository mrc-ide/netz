#' Fit distribution to match target usage using a sequential algorithm. This 
#' assumes an exponentially distributed net loss function and randomly 
#' correlated net distribution. This is faster than the nloptr approach,
#' but does rely on consistent data quality as latter values will be influenced
#' by earlier values.
#'
#' @param target_usage Target usage
#' @param target_usage_timesteps Target usage time points
#' @param distribution_timesteps A vector of distribution time steps
#' @param distribution_lower Lower bound on distributions (default = 0)
#' @param distribution_upper Upper bound on distribution (default = 1)
#' @param mean_retention The average duration of net retention (days)
fit_usage_sequential <- function(
    target_usage,
    target_usage_timesteps,
    distribution_timesteps, 
    distribution_lower = rep(0, length(distribution_timesteps)), 
    distribution_upper = rep(1, length(distribution_timesteps)),
    mean_retention = 365 * 5
){
  loss_rate <- 1 / mean_retention
  distribution <- rep(0, length(distribution_timesteps))
  for(t in 1:length(distribution_timesteps)){
    # Usage at time point of next distribution
    put <- population_usage_t(distribution_timesteps[t], distribution, distribution_timesteps, mean_retention)
    # Find next target usage
    time_offset <- target_usage_timesteps  - distribution_timesteps[t]
    if(max(time_offset) < 0){
      distribution[t] <- NA
    } else {
      nearest <- min(time_offset[time_offset >= 0])
      index <- which(time_offset == nearest)
      start_point <- target_usage[index] / exp(-loss_rate * time_offset[index])
      distribution[t] <- 1 - (1 - start_point) / (1 - put)
      distribution[t] <- min(distribution_upper[t], distribution[t])
      distribution[t] <- max(distribution_lower[t], distribution[t])
    }
  }
  return(distribution)
}

#' Internal objective function for least-squares non-linear fitting of target usage
#'
#' @param distribution A vector of proposed distribution
#' @inheritParams fit_usage
#'
#' @return Sum of squared difference between proposed usage and target usage
fit_usage_objective <- function(
    distribution,
    distribution_timesteps,
    target_usage,
    target_usage_timesteps,
    mean_retention
){
  pu <- population_usage_t(
    timesteps = target_usage_timesteps,
    distribution = distribution,
    distribution_timsteps = distribution_timesteps,
    mean_retention = mean_retention
  )
  sum((pu - target_usage) ^ 2)
}

#' Fit distribution to match target usage using non-linear optimisation. This 
#' assumes an exponentially distributed net loss function and randomly 
#' correlated net distribution. 
#'
#' @param target_usage Target usage
#' @param target_usage_timesteps Target usage time points
#' @param distribution_timesteps A vector of distribution time steps
#' @param distribution_init Starting distribution for optimiation
#' @param distribution_lower Lower bound on distributions (default = 0)
#' @param distribution_upper Upper bound on distribution (default = 1)
#' @param timesteps Net loss vector
#' @param half_life Net retention half life
#' @param ... Further arguments to pass to the \link[nloptr]{cobyla}
#' 
#' 
#' @return Non-linear optimisation fit output
#' @export
fit_usage <- function(
    target_usage,
    target_usage_timesteps,
    distribution_timesteps,
    distribution_init = rep(0, length(distribution_timesteps)),
    distribution_lower = rep(0, length(distribution_timesteps)),
    distribution_upper = rep(1, length(distribution_timesteps)),
    timesteps = max(c(target_usage_timesteps, distribution_timesteps)),
    half_life = 5 * 365,
    ...
){
  
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
                 mean_retention = mean_retention,
                 ...)
  
}
