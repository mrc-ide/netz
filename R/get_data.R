#' Get half life
#'
#' @param iso3c ISO3c code.
#' @param not_found_fn A function to summarise across all
#' available half-life estimates to produce an estimate
#' for countries not in the dataset
#' @param ... Further arguments passed to not_found_fn
#'
#' @export
#' @return Half life
get_halflife <- function(iso3c, not_found_fn = stats::median, ...){
  hld <- netz::halflife
  
  iso_in_dataset <- iso3c %in% hld$iso3c
  
  if(iso_in_dataset){
    half_life <- hld[hld$iso3c == iso3c, "half_life"]
  } else {
    half_life <- not_found_fn(hld$half_life, ...)
  }
  
  return(half_life)
}

#' Get usage rate
#'
#' @param iso3c ISO3c code.
#' @param not_found_fn A function to summarise across all
#' available half-life estimates to produce an estimate
#' for countries not in the dataset
#' @param ... Further arguments passed to not_found_fn
#' 
#' @export
#' @return Usage rate
get_usage_rate <- function(iso3c, not_found_fn = stats::median, ...){
  ur <- netz::usage_rate
  
  iso_in_dataset <- iso3c %in% ur$iso3c
  
  if(iso_in_dataset){
    usage_rate <- ur[ur$iso3c == iso3c, "usage_rate"]
  } else {
    usage_rate <- not_found_fn(ur$usage_rate, ...)
  }
  
  return(usage_rate)
}

