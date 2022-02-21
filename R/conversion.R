#' Predict the nets per capita associated witha given access
#'
#' @param access A single or vector of access values
#' @param extrapolate If TRUE, access values outside of the observed range will
#' be included. For access values below observed values this is an extrapolation
#' of the loess smooth between the lowest observed value and the (0, 0) point. For
#' access values above observed values this is a linear extrapolation of the trend
#' int he top 5% of observed access values.
#'
#' @return Predicted nets per capita
#' @export
access_to_npc <- function(access, extrapolate = TRUE){
  smooth <- loess_fits$access_loess_raw
  if(extrapolate){
    smooth <- loess_fits$access_loess_extrapolate
  }
  pred <- unname(predict(smooth, newdata = data.frame(access_mean = access)))
  return(pred)
}

#' Predict the access nets associated with a given nets per capita
#'
#' @param access A single or vector of nets per capita
#' @inheritParams access_to_npc
#'
#' @return Predicted access
#' @export
npc_to_access <- function(npc, extrapolate = TRUE){
  smooth <- loess_fits$access_loess_raw
  if(extrapolate){
    smooth <- loess_fits$access_loess_extrapolate
  }
  access <- seq(0, 1, 0.01)
  pred <- unname(predict(smooth, newdata = data.frame(access_mean = access)))
  npc <- vapply(npc, function(x, pred, access){
    access[which.min(abs(pred - x))]
  }, pred = pred, access = access, FUN.VALUE = numeric(1))
  npc
}

#' Convert usage to access
#'
#' @param usage A single value or vector of desired target usages to model.
#' @param use_rate A single value or vector of usage rates.
#'
#' @return Access
#' @export
usage_to_access <- function(usage, use_rate){
  access <- usage / use_rate
  if(any(access > 1)){
    warning("Target usage(s) cannot be achieved with input usage_rates - return NA")
    access[access > 1] < NA
  }
  return(access)
}

#' Convert access to usage
#'
#' @param usage A single value or vector of access.
#' @inheritParams usage_to_access
#'
#' @return Usage
#' @export
access_to_usage <- function(access, use_rate){
  usage <- access * use_rate
  return(usage)
}

#' Convert nets per capita to annual nets per capital delivered
#'
#' @param npc Nets per capita
#' @param distribution_freq A single distribution frequency of nets in days. Default = 3*365 (3 years).
#' @param net_loss_function Option to choose between exponential net loss (net_loss_exp) or
#' the smooth-compact net loss function from Bertozzi-Villa, Amelia, et al.
#' Nature communications 12.1 (2021): 1-12 (net_loss_map). Default = net_loss_map.
#' @param ... additional arguments for the net_loss_function
#' 
#' @return Annual nets per capita delivered
#' @export
npc_to_anpcd <- function(npc, distribution_freq = 3 * 365, net_loss_function = net_loss_map, ...){
  npc * mean(net_loss_function(t = seq(0, distribution_freq, 1), ...))
}

#' Convert annual nets per capital delivered to nets per capita
#'
#' @param anpcd Annual nets per capital delivered
#' @inheritParams npc_to_anpcd
#' 
#' @return Nets per capita
#' @export
anpcd_to_npc <- function(anpcd, distribution_freq = 3 * 365, net_loss_function = net_loss_map, ...){
  anpcd / mean(net_loss_function(t = seq(0, distribution_freq, 1), ...))
}