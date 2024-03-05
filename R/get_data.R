#' Get half life
#'
#' @param iso3c ISO3c code. If NA will return median half life estimate
#'
#' @export
#' @return Half life
get_halflife <- function(iso3c = NA){
  hld <- netz::halflife
  
  if(is.na(iso3c)){
    half_life <- stats::median(hld$half_life)
  } else {
    if(!iso3c %in% hld$iso3c){
      stop("ISO code not found")
    }
    half_life <- hld[hld$iso3c == iso3c, "half_life"]
  }
  
  return(half_life)
}

#' Get usage rate
#'
#' @param iso3c ISO3c code. If NA will return median usage rate estimate
#'
#' @export
#' @return Usage rate
get_usage_rate <- function(iso3c = NA){
  ur <- netz::usage_rate
  
  if(is.na(iso3c)){
    usage_rate <- stats::median(ur$usage_rate)
  } else {
    if(!iso3c %in% ur$iso3c){
      stop("ISO code not found")
    }
    usage_rate <- ur[ur$iso3c == iso3c, "usage_rate"]
  }
  
  return(usage_rate)
}

