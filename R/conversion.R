#' Predict the nets per capita associated witha given access
#'
#' @param access A single or vector of access values
#' @param type The npc to access model to use. This may be:
#' \itemize{
#'  \item{"loess"}{: a loess fit to observed access-npc data}
#'  \item{"linear"}{: a linear fit to the observed access-npc data, fitted to the trend for observeation with access < 0.5}
#' }
#' @param people_per_net Assumed number of people who can use 1 net if type = "linear"
#'
#' @return Predicted nets per capita
#' @export
access_to_crop <- function(access, type = "loess", people_per_net = 1.66856){
  if(any(access < 0 | access > 1, na.rm = TRUE)){
    stop("access must be between 0 and 1")
  }
  if(!type %in% c("loess", "linear")){
    stop("type must be one of: loess, linear")
  }
  
  if(type == "linear"){
    crop <- access / people_per_net
  } else {
    smooth <- netz::npc_fits[["loess"]]
    crop <- unname(stats::predict(smooth, newdata = data.frame(access_mean = access)))
    crop[access < 0.4] <- access[access < 0.4] / 1.66856
  }
  
  return(crop)
}

#' Predict the access nets associated with a given nets per capita
#'
#' @param crop A single or vector of nets per capita
#' @inheritParams access_to_crop
#'
#' @return Predicted access
#' @export
crop_to_access <- function(crop, type = "loess", people_per_net = 1.66856){
  if(!type %in% c("loess", "linear")){
    stop("type must be one of: loess, linear")
  }
  access_search <- seq(0, 1, 0.001)
  pred <- access_to_crop(access = access_search, type = type, people_per_net = people_per_net)
  access <- stats::approx(x = pred, y = access_search, xout = crop)$y
  access[crop == 0] <- 0
  return(access)
}

#' Convert usage to access
#'
#' @param usage A single value or vector of desired target usages to model.
#' @param use_rate A single value or vector of usage rates.
#' @param max_access_value Value to use if resulting access > 1
#'
#' @return Access
#' @export
usage_to_access <- function(usage, use_rate, max_access_value = 1){
  if(any(usage < 0 | usage > 1, na.rm = TRUE)){
    stop("usage must be between 0 and 1")
  }
  
  access <- usage / use_rate
  access[access > 1] <- max_access_value

  access[usage == 0] <- 0
  return(access)
}

#' Convert access to usage
#'
#' @param access A single value or vector of access.
#' @inheritParams usage_to_access
#'
#' @return Usage
#' @export
access_to_usage <- function(access, use_rate){
  if(any(access < 0 | access > 1, na.rm = TRUE)){
    stop("access must be between 0 and 1")
  }
  
  usage <- access * use_rate
  usage[access == 0] <- 0
  
  return(usage)
}

#' Convert nets per capita to annual nets per capital delivered
#'
#' @param crop Vector of nets per capita
#' @param crop_timesteps Timesteps of crop estimates
#' @param min_distribution Vector of optional minimum bounds on distributions
#' @param max_distribution Vector of optional maximum bounds on distributions
#' @inheritParams distribution_to_crop
#' 
#' @return Annual nets per capita delivered
#' @export
crop_to_distribution <- function(
    crop,
    crop_timesteps,
    distribution_timesteps,
    net_loss_function = netz::net_loss_map,
    min_distribution = NULL,
    max_distribution = NULL,
    ...){
  
  max_time <- max(crop_timesteps)
  n_distributions <- length(distribution_timesteps)
  if(is.null(min_distribution)){
    min_distribution <- rep(0, n_distributions) 
  }
  if(is.null(max_distribution)){
    max_distribution <- rep(2, n_distributions) 
  }
  if(length(min_distribution) != n_distributions | length(max_distribution) != n_distributions){
    stop("min and max distribution vectors must be of length(distribution_timesteps)")
  }
  
  
  decay <- net_loss_function(
    t = 0:max_time,
    ...
  )
  
  crop_matrix <- matrix(
    data = 0,
    nrow = max_time,
    ncol = n_distributions
  )
  
  distribution <- rep(NA, n_distributions)
  
  for(i in 1:n_distributions){
    distribution_t <- distribution_timesteps[i]
    # Find the next crop data point after the current distribution day
    target_t <- crop_timesteps[crop_timesteps >= distribution_t][1]
    # Finish if no more crop estimates available
    if(is.na(target_t)){
      break
    }
    target_index <- which(crop_timesteps == target_t)
    
    current <- rowSums(crop_matrix)[target_t]
    target <- crop[target_index]
    # Need to account for some nets replacing others (randomly)
    # delta <- 1 - ((1 - target) / (1 - current))
    delta <- target - current
    # Adjust for the target day and distribution day being different
    delta <- delta * (decay[1] / decay[target_t - distribution_t + 1])
    delta <- max(delta, min_distribution[i])
    delta <- min(delta, max_distribution[i])
    distribution[i] <- delta
    
    dist_decay <- (delta * decay)
    crop_matrix[distribution_t:max_time,i] <- dist_decay[1:(max_time - distribution_t + 1)]
  }
  return(distribution)
}

#' Convert annual nets per capital delivered to nets per capita
#'
#' @param distribution Nets per capital delivered
#' @param distribution_timesteps Timesteps of distributions
#' @param crop_timesteps Timesteps to return crop estimates
#' @param net_loss_function Option to choose between exponential net loss (net_loss_exp) or
#' the smooth-compact net loss function from Bertozzi-Villa, Amelia, et al.
#' Nature communications 12.1 (2021): 1-12 (net_loss_map). Default = net_loss_map.
#' @param ... additional arguments for the net_loss_function
#' 
#' @return Nets per capita
#' @export
distribution_to_crop <- function(
    distribution,
    distribution_timesteps,
    crop_timesteps,
    net_loss_function = netz::net_loss_map,
    ...){
  
  max_time <- max(crop_timesteps)
  n_distributions <- length(distribution_timesteps)
  
  crop_matrix <- matrix(
    data = 0,
    nrow = max_time,
    ncol = n_distributions
  )
  
  decay <- net_loss_function(
    t = 0:max_time,
    ...
  )
  
  for(i in 1:n_distributions){
    distribution_t <- distribution_timesteps[i]
    dist_decay <- (distribution[i] * decay)
    crop_matrix[distribution_t:max_time,i] <- dist_decay[1:(max_time - distribution_t + 1)]
  }
  crop <- rowSums(crop_matrix)[crop_timesteps]
  return(crop)
}