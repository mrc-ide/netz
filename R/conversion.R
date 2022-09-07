#' Predict the nets per capita associated witha given access
#'
#' @param access A single or vector of access values
#' @param type The npc to access model to use. This may be:
#' \itemize{
#'  \item{"loess"}{: a loess fit to observed access-npc data}
#'  \item{"loess_extrapolate"}{: a loess fit to observed access-npc data with trends extrapolated above and below 
#'  observed values.}
#'  \item{"linear"}{: a linear fit to the observed access-npc data, fitted to the trend for observeation with access < 0.5}
#' }
#'
#' @return Predicted nets per capita
#' @export
access_to_crop <- function(access, type = "loess"){
  if(any(access < 0 | access > 1, na.rm = TRUE)){
    stop("access must be between 0 and 1")
  }
  if(!type %in% c("loess", "loess_extrapolate", "linear")){
    stop("type must be one of: loess, loess_extrapolate or linear")
  }
  smooth <- netz::npc_fits[[type]]
  pred <- unname(stats::predict(smooth, newdata = data.frame(access_mean = access)))
  return(pred)
}

#' Predict the access nets associated with a given nets per capita
#'
#' @param crop A single or vector of nets per capita
#' @inheritParams access_to_crop
#'
#' @return Predicted access
#' @export
crop_to_access <- function(crop, type = "loess"){
  if(!type %in% c("loess", "loess_extrapolate", "linear")){
    stop("type must be one of: loess, loess_extrapolate or linear")
  }
  smooth <- netz::npc_fits[[type]]
  access <- seq(0, 1, 0.001)
  pred <- access_to_crop(access, type)
  access_out <- stats::approx(x = pred, y = access, xout = crop)$y
  access_out
}

#' Convert usage to access
#'
#' @param usage A single value or vector of desired target usages to model.
#' @param use_rate A single value or vector of usage rates.
#'
#' @return Access
#' @export
usage_to_access <- function(usage, use_rate){
  if(any(usage < 0 | usage > 1, na.rm = TRUE)){
    stop("usage must be between 0 and 1")
  }
  
  access <- usage / use_rate
  if(any(access > 1, na.rm = TRUE)){
    warning("Target usage(s) cannot be achieved with input usage_rates - return NA")
    access[access > 1] <- NA
  }
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
  return(usage)
}

#' Convert nets per capita to annual nets per capital delivered
#'
#' @param crop Nets per capita
#' @param distribution_freq A single distribution frequency of nets in days. Default = 3*365 (3 years).
#' @param half_life Net loss half life (days)
#' @param net_loss_function Option to choose between exponential net loss (net_loss_exp) or
#' the smooth-compact net loss function from Bertozzi-Villa, Amelia, et al.
#' Nature communications 12.1 (2021): 1-12 (net_loss_map). Default = net_loss_map.
#' @param ... additional arguments for the net_loss_function
#' 
#' @return Annual nets per capita delivered
#' @export
crop_to_distribution <- function(crop, distribution_freq, half_life, net_loss_function = net_loss_map, ...){
  if(any(crop < 0, na.rm = TRUE)){
    stop("crop must be > 0")
  }
  if(any(distribution_freq < 0, na.rm = TRUE)){
    stop("distribution_freq must be > 0")
  }
  if(any(half_life < 0, na.rm = TRUE)){
    stop("half_life must be > 0")
  }
  
  crop / ((distribution_freq / 365) * mapply(function(distribution_freq, half_life){
    nl <- net_loss_function(t = 0:(100*365), half_life = half_life, ...)
    index <- 1:length(nl) %% distribution_freq
    mean(tapply(nl, index, sum))
  }, distribution_freq = distribution_freq, half_life = half_life))
}

#' Convert annual nets per capital delivered to nets per capita
#'
#' @param distribution Annual nets per capital delivered
#' @inheritParams crop_to_distribution
#' 
#' @return Nets per capita
#' @export
distribution_to_crop <- function(distribution, distribution_freq, half_life, net_loss_function = net_loss_map, ...){
  if(any(distribution < 0, na.rm = TRUE)){
    stop("distribution must be > 0")
  }
  if(any(distribution_freq < 0, na.rm = TRUE)){
    stop("distribution_freq must be > 0")
  }
  if(any(half_life < 0, na.rm = TRUE)){
    stop("half_life must be > 0")
  }
  
  distribution * (distribution_freq / 365) * mapply(function(distribution_freq, half_life){
    nl <- net_loss_function(t = 0:(100*365), half_life = half_life, ...)
    index <- 1:length(nl) %% distribution_freq
    mean(tapply(nl, index, sum))
  }, distribution_freq = distribution_freq, half_life = half_life)
}