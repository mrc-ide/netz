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
#' @details Note, the estimation works sequentially in time order, so if there
#' are multiple distribution steps specified between target usage timestep they will
#' be inferred sequentially. You can mange this behaviour with the `distribution_lower` and
#' `distribution_upper` arguments.
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
  if (is.unsorted(distribution_timesteps)) {
    stop("Distribution timesteps must be increasing")
  }
  if (is.unsorted(usage_timesteps)) {
    stop("Usage timesteps must be increasing")
  }
  if (any(usage < 0 | usage > 1)) {
    stop("Usage must be between 0 and 1")
  }
  if (any(distribution_lower < 0) || any(distribution_lower > 1)) {
    stop("All distribution lower values must be between 0 and 1")
  }
  if (any(distribution_upper < 0) || any(distribution_upper > 1)) {
    stop("All distribution upper values must be between 0 and 1")
  }
  if (
    length(distribution_lower) != length(distribution_timesteps) ||
      length(distribution_upper) != length(distribution_timesteps)
  ) {
    stop("distribution timesteps, lower bounds and upper bounds must have same length")
  }
  if (length(usage) != length(usage_timesteps)) {
    stop("Usage and usage timesteps must be the same length")
  }

  distribution <- rep(0, length(distribution_timesteps))
  prop_kept <- rep(0, length(distribution_timesteps))

  for (t in seq_along(distribution_timesteps)) {
    index <- which(usage_timesteps >= distribution_timesteps[t])[1]
    if (is.na(index)) {
      distribution[t:length(distribution)] <- NA
      break
    }
    net_age <- usage_timesteps[index] - distribution_timesteps
    prev_dist <- seq_len(t - 1)

    previous_usage <- sum(prop_kept[prev_dist] * net_loss_function(net_age[prev_dist], ...))
    retained <- net_loss_function(net_age[t], ...)

    # Solving usage = (1-dist)*previous_usage + dist * retained for dist 
    delta_usage <- usage[index] - previous_usage
    delta_denom <- retained - previous_usage

    if (delta_denom <= 0) {
      distribution[t] <- distribution_lower[t]
    } else {
      distribution[t] <- delta_usage / delta_denom
    }

    if (distribution_upper[t] < distribution[t]) {
      warning(sprintf("At distribution %d required coverage (%.2f) is greater than upper limit (%.2f)",
        t, distribution[t], distribution_upper[t]
      ))
    }
    if (distribution_lower[t] > distribution[t]) {
      warning(sprintf("At distribution %d required coverage (%.2f) is less than lower limit (%.2f)",
        t, distribution[t], distribution_lower[t]
      ))
    }

    distribution[t] <- min(distribution_upper[t], distribution[t])
    distribution[t] <- max(distribution_lower[t], distribution[t])

    prop_kept <- prop_kept * (1 - distribution[t])
    prop_kept[t] <- distribution[t]
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
  if (is.unsorted(distribution_timesteps)) {
    stop("Distribution timesteps must be increasing")
  }
  if (is.unsorted(usage_timesteps)) {
    stop("Usage timesteps must be increasing")
  }
  usage <- rep(0, length(usage_timesteps))

  for (t in seq_along(usage_timesteps)){
    prev_dist <- which(distribution_timesteps <= usage_timesteps[t])
    net_age <- usage_timesteps[t] - distribution_timesteps

    prop_kept <-  rep(0, length(distribution_timesteps))
    for (i in prev_dist) {
      prop_kept <- prop_kept * (1 - distribution[i])
      prop_kept[i] <- distribution[i]
    }
    usage[t] <- sum(prop_kept[prev_dist] * net_loss_function(net_age[prev_dist], ...))
  }
  return(usage)
}
