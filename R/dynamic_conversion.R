#' Dynamic crop to distribution estimation
#' 
#' Estimates the distributions required to achieve a given, non-equilibrium
#' crop.
#'
#' @param crop Vector fo annual crop estimates
#' @param net_loss_function Net loss function
#' @param ... Additional parameters to the net loss function
#'
#' @return Annual distribution vector
#' @export
crop_to_distribution_dynamic <- function(crop, net_loss_function, ...){
  years <- length(crop)
  # Assume distribution on first day of the year
  xt <- 365 *  1:years - 364
  # Assume target measured at the mid point of the year
  target_t <- xt + 182
  dist <- dynamic_opti(target = crop, target_t = target_t, xt = xt, loss_function = net_loss_function, ...)$par
  dist[crop == 0] <- 0
  return(dist)
}

#' Dynamic distribution to crop estimation
#' 
#' Estimates the crop resulting from a given, non-equilibrium
#' distribution
#' 
#' @param distribution Vector of annual distribution estimates
#' @param net_loss_function Net loss function
#' @param ... Additional parameters to the net loss function
#'
#' @return Annual crop vector
#' @export
distribution_to_crop_dynamic <- function(distribution, net_loss_function, ...){
  years <- length(distribution)
  # Assume distribution on first day of the year
  xt <- 365 *  1:years - 364
  # Assume target measured at the mid point of the year
  target_t <- xt + 182
  crop <- dynamic_crop(x = distribution, xt = xt, timesteps = years * 365,
                       loss_function = net_loss_function, ...)
  crop <- crop[target_t]
  return(crop)
}

#' Shift the net loss function
#'
#' @param dt start time
#' @param l net loss vector
#' @param t timesteps
#'
#' @return shifted, trimmed net loss
shifted <- function(dt, l, t){
  c(rep(0, dt), l)[1:t]
}

#' Cumulative crop
#'
#' @param x Input distribution
#' @param xt Input time steps
#' @param timesteps Total timesteps
#' @param loss_function Net loss function
#' @param ... Additional arguments to net loss function
#'
#' @return Sum of multiple, shifted net loss
dynamic_crop <- function(x, xt, timesteps, loss_function, ...){
  nl <- loss_function(t = 1:timesteps, ...)
  loss_matrix <- sapply(xt, shifted, l = nl, t = timesteps)
  x_matrix <- loss_matrix %*% diag(x)
  y <- rowSums(x_matrix)
  return(y)
}

#' Cumu objetive function
#'
#' @inheritParams dynamic_crop
#' @param target Target values
#' @param target_t Target value timesteps
objective <- function (x, xt, target, target_t, timesteps, loss_function, ...) {
  pu <- dynamic_crop(x = x, xt = xt, timesteps = timesteps, loss_function = loss_function, ...)
  sum((pu[target_t] - target)^2)
}

#' Optimise distribution
#'
#' @inheritParams objective
#' @param x_upper Upper bound on x
#' @param x_init Initial values of x
#' @param x_lower Lower bound on x
#'
#' @return Optimised distribution to achieve a given crop
dynamic_opti <- function (target, target_t, 
                  xt, 
                  x_upper = rep(3, length(xt)),
                  x_init = rep(0, length(xt)), 
                  x_lower = rep(0, length(xt)), 
                  timesteps = max(c(target_t, xt)), 
                  loss_function, ...) 
{
  if (any(x_lower < 0) | any(x_lower > 
                             1)) {
    stop("All distribution lower values must be between 0 and 1")
  }
  if (any(x_upper < 0)) {
    stop("All x_upper values must be greater than 0")
  }
  if (length(x_lower) != length(xt) | 
      length(x_upper) != length(xt)) {
    stop("distribution_timesteps, x_lower and x_upper must have equal lengths")
  }
  if (length(target) != length(target_t)) {
    stop("target and target_t must be the same length")
  }
  nloptr::cobyla(x0 = x_init, fn = objective, 
                 lower = x_lower, upper = x_upper, 
                 xt = xt, target = target, 
                 target_t = target_t, timesteps = timesteps, 
                 loss_function = loss_function,
                 ...)
}
