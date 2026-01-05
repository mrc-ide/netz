#' Estimate the malariasimulation input distribution from usage
#' assumes a specified net loss function and randomly
#' correlated net distribution.
#'
#'
#' @param usage Target usage
#' @param usage_timesteps Target usage time points
#' @param distribution_timesteps A vector of distribution time steps
#' @param distribution_lower Lower bound on distributions (default = 0)
#' @param distribution_upper Upper bound on distribution (default = 1)
#' @param net_loss_function Function to calculate net retention over time.
#'   Should take time as first argument and return proportion retained.
#'   Defaults to net_loss_exp for backward compatibility.
#' @param... Additional arguments passed to net_loss_function
#'
#' @export
usage_to_model_distribution <- function(
  usage,
  usage_timesteps,
  distribution_timesteps,
  distribution_lower = rep(0, length(distribution_timesteps)),
  distribution_upper = rep(1, length(distribution_timesteps)),
  net_loss_function = net_loss_exp,
  ...
) {
  if (any(distribution_lower < 0) | any(distribution_lower > 1)) {
    stop("All distribution lower values must be between 0 and 1")
  }
  if (any(distribution_upper < 0) | any(distribution_upper > 1)) {
    stop("All distribution upper values must be between 0 and 1")
  }
  if (
    length(distribution_lower) != length(distribution_timesteps) |
      length(distribution_upper) != length(distribution_timesteps)
  ) {
    stop(
      "distribution_timesteps, distribution_lower and distribution_upper must have equal lengths"
    )
  }
  if (length(usage) != length(usage_timesteps)) {
    stop("Usage and usage timesteps must be the same length")
  }

  distribution <- rep(0, length(distribution_timesteps))
  for (t in 1:length(distribution_timesteps)) {
    # Usage at time point of next distribution
    put <- model_distribution_to_usage(
      usage_timesteps = distribution_timesteps[t],
      distribution = distribution,
      distribution_timesteps = distribution_timesteps,
      net_loss_function = net_loss_function,
      ...
    )

    # Find next target usage
    time_offset <- usage_timesteps - distribution_timesteps[t]
    if (max(time_offset) < 0) {
      distribution[t] <- NA
    } else {
      nearest <- min(time_offset[time_offset >= 0])
      index <- which(time_offset == nearest)
      start_point <- usage[index] / net_loss_function(time_offset[index], ...)
      distribution[t] <- 1 - (1 - start_point) / (1 - put)
      distribution[t] <- min(distribution_upper[t], distribution[t])
      distribution[t] <- max(distribution_lower[t], distribution[t])
    }
  }
  return(distribution)
}

#' Estimate the population-level bed net usage at given times for a set of
#' bed net distributions at given times. This assumes a specified rate of net loss
#' and that recipients of multiple rounds are random.
#'
#' @inheritParams usage_to_model_distribution
#' @param distribution Vector of model distribution (% of total population)
#' @param net_loss_function Function to calculate net retention over time.
#'   Should take time as first argument and return proportion retained.
#' @param... Additional arguments passed to net_loss_function
#'
#' @return Usage estimates at timepoints
#' @export
model_distribution_to_usage <- function(
  usage_timesteps,
  distribution,
  distribution_timesteps,
  net_loss_function = net_loss_exp,
  ...
) {
  # Estimate the cumulative usage at distribution time points
  cumulative_usage <- distribution[1]
  if (length(distribution_timesteps) > 1) {
    for (t in 2:length(distribution_timesteps)) {
      time_offset <- distribution_timesteps[t] - distribution_timesteps[t - 1]
      remaining <- cumulative_usage[t - 1] * net_loss_function(time_offset, ...)
      cumulative_usage[t] <- 1 - (1 - remaining) * (1 - distribution[t])
    }
  }

  # Estimate the usage at target time points
  usage <- c()
  for (t in seq_along(usage_timesteps)) {
    time_offset <- usage_timesteps[t] - distribution_timesteps
    if (max(time_offset) < 0) {
      usage[t] <- 0
    } else {
      nearest <- min(time_offset[time_offset >= 0])
      index <- which(time_offset == nearest)
      usage[t] <- cumulative_usage[index] *
        net_loss_function(time_offset[index], ...)
    }
  }
  return(usage)
}
