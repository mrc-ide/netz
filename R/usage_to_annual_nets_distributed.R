#' Convert bednet usage to annual nets distributed per capita
#'
#' Calculates the annual number of nets to distribute per capita to achieve a desired
#' net usage.
#' Usage is converted to access using the input use rate(s), which is then
#' converted to nets per capita using the observed relationship between
#' access and nets per capita across Africa, as described in
#' Bertozzi-Villa, Amelia, et al. Nature communications 12.1 (2021): 1-12.
#' Nets per capita are converted to nets distributed annually by accounting for
#' net loss over time within a given distribution cycle.
#'
#' @param target_usage A single value or vector of desired target usages to model.
#' @param distribution_freq A single distribution frequency of nets in days. Default = 3*365 (3 years).
#' @param use_rate_data Dataset of country-specific net use rates obtained with prepare_data() or
#' a value/vector of net use rates. Default = prepare_data()$use_rate_by_country. Can also
#' be limited to specific ISO3 codes.
#' @param half_life_data Dataset of country-specific net half lives obtained with prepare_data() or
#' a value/vector of net half lives (median retention times) in days. Can also
#' be limited to specific ISO3 codes.
#' @param access_vs_npc_data Dataset of net access against nets per capita (Loess curve)
#' obtained with prepare_data(). Default = prepare_data()$loess_for_prediction. Needs to
#' include all countries/ISO3 codes.
#' @param extrapolate_npc Option to extrapolate target nets per capita beyond fitted
#' Loess curve. Default = "loess" for continuation of Loess curve trend. Use option
#' "linear" for linear extrapolation. Use option FALSE to return NA outputs
#' for any target usages exceeding currently observed levels.
#' @param net_loss_function Option to choose between exponential net loss (net_loss_exp) or
#' the smooth-compact net loss function from Bertozzi-Villa, Amelia, et al.
#' Nature communications 12.1 (2021): 1-12 (net_loss_map). Default = net_loss_map.
#' @param k Fixed rate parameter of the MAP function of net loss (net_loss_map()), used to
#' estimate the median net retention times by country. Default = 20 from
#' Bertozzi-Villa, Amelia, et al. Nature communications 12.1 (2021): 1-12.
#'
#' @return Dataframe of access, nets per capita and annual nets distributed per capita
#' for the desired steady-state target usage by country
#'
#' @importFrom stats 'approx' 'reshape' 'loess' 'predict' 'loess.control'
#' @export
convert_usage_to_annual_nets_distributed <- function(target_usage, distribution_freq = 3 * 365,
                                                     use_rate_data = prepare_data()$use_rate_by_country,
                                                     half_life_data = prepare_data()$half_life_data,
                                                     access_vs_npc_data = prepare_data()$loess_for_prediction,
                                                     extrapolate_npc = "loess",
                                                     net_loss_function = net_loss_map,
                                                     k = 20) {

  ### 1. Convert target usage to nets per capita ###
  usage_to_npc <- convert_usage_to_npc(
    target_usage = target_usage,
    use_rate_data = use_rate_data,
    access_vs_npc_data = access_vs_npc_data,
    extrapolate_npc = extrapolate_npc
  )

  ### 2. Convert equilibrium nets per capita to annual nets distributed per capita ###
  nets_distributed <- convert_npc_to_annual_nets_distributed(
    usage_to_npc_output = usage_to_npc,
    distribution_freq = distribution_freq,
    half_life_data = half_life_data,
    net_loss_function = net_loss_function,
    k = k
  )

  return(nets_distributed)
}

#' Convert bednet usage to nets per capita
#'
#' Calculates the annual number of nets needed per capita to achieve a desired
#' net usage.
#' Usage is converted to access using the input use rate(s), which is then
#' converted to nets per capita using the observed relationship between
#' access and nets per capita across Africa, as described in
#' Bertozzi-Villa, Amelia, et al. Nature communications 12.1 (2021): 1-12.
#'
#' @inheritParams convert_usage_to_annual_nets_distributed
#' @export
convert_usage_to_npc <- function(target_usage,
                                 use_rate_data = prepare_data()$use_rate_by_country,
                                 access_vs_npc_data = prepare_data()$loess_for_prediction,
                                 extrapolate_npc = "loess") {

  ### 1. Convert target usage to nets per capita ###

  if (class(use_rate_data) == "numeric") {
    use_rate_data <- data.frame(iso3 = NA, year = NA, use_rate = use_rate_data)
  }

  if (extrapolate_npc == "linear") {
    # Add datapoint of (1,1) to allow linear extrapolation of nets per capita
    # for access levels beyond those observed
    loess_for_prediction <- rbind(
      access_vs_npc_data,
      data.frame(
        iso3 = NA, month = NA, access = 1,
        percapita_nets = 1, loess_predicted_access = 1
      )
    )
    print("Access-nets per capita curve is extrapolated linearly beyond observed levels")
  } else if (extrapolate_npc == "loess") {
    curve_fit <- loess(access ~ percapita_nets,
      data = access_vs_npc_data,
      control = loess.control(surface = "direct")
    )
    extrapolate_access <- data.frame(
      iso3 = NA, month = NA, access = NA, percapita_nets =
        seq(ceiling(max(access_vs_npc_data$percapita_nets) * 100) / 100, 1, 0.01)
    )
    extrapolate_access$loess_predicted_access <- predict(curve_fit, extrapolate_access$percapita_nets)
    loess_for_prediction <- rbind(access_vs_npc_data, extrapolate_access)
    print("Access-nets per capita curve is extrapolated beyond observed levels")
  } else if (extrapolate_npc == FALSE) {
    loess_for_prediction <- access_vs_npc_data
    print("No extrapolation beyond observed access-nets per capita relationship - return NA")
  } else {
    stop("extrapolate_npc must be one of: loess, linear, FALSE")
  }

  max_usage <- use_rate_data[, c("iso3", "use_rate")]
  names(max_usage)[names(max_usage) == "use_rate"] <- "maximum_usage"

  if (any(target_usage <= max(use_rate_data[, "use_rate"])) == FALSE) {
    print(max_usage)
    stop("Target usage(s) cannot be achieved with any of the input use rates. Refer to maximum usage above.")
  } else if (any(target_usage > min(use_rate_data[, "use_rate"]))) {
    print(max_usage)
    warning("Target usage(s) cannot be achieved with some of the input use rates - return NA. Refer to maximum usage above.")
  }

  targets_all <- lapply(target_usage, function(this_use) {
    targets_dt <- use_rate_data[, c("iso3", "year", "use_rate")]
    targets_dt$target_use <- this_use
    # Convert target usage to target access: access = usage/use rate
    # since use rate = the proportion of people with access to a net who slept under it
    targets_dt$target_access <- targets_dt$target_use / targets_dt$use_rate
    targets_dt <- targets_dt[order(targets_dt$target_access), ]
    targets_dt$target_percapita_nets <- NA
    targets_dt$target_percapita_nets[
      targets_dt$target_access <= max(loess_for_prediction$loess_predicted_access)
    ] <-
      approx(
        loess_for_prediction$loess_predicted_access,
        loess_for_prediction$percapita_nets,
        targets_dt$target_access[targets_dt$target_access <= max(loess_for_prediction$loess_predicted_access)]
      )$y
    # Relationship between access and NPC is aggregated for all countries,
    # but use rate to convert usage to access is country-specific (derived from spatiotemporal regression?)
    return(targets_dt[order(targets_dt$iso3), ])
  })
  targets_all <- do.call("rbind", targets_all)
  targets_all$target_access[targets_all$target_access > 1] <- NA
  # Set access to NA where it is over 1 and excluded from conversion

  return(targets_all)
}

#' Convert nets per capita to annual nets distributed per capita
#'
#' Nets per capita are converted to nets distributed annually by accounting for
#' net loss over time within a given distribution cycle.
#'
#' @param usage_to_npc_output Dataframe of nets per capita for given target usage (output from convert_usage_to_npc())
#' @inheritParams convert_usage_to_annual_nets_distributed
#' @export
convert_npc_to_annual_nets_distributed <- function(usage_to_npc_output,
                                                   distribution_freq = 3 * 365,
                                                   half_life_data = prepare_data()$half_life_data,
                                                   net_loss_function = net_loss_map,
                                                   k = 20) {


  ### 2. Convert equilibrium nets per capita to annual nets distributed per capita ###

  # half life class numeric and usage_to_npc iso = NA
  # half life class NOT numeric and usage to npc iso NOT NA
  # Check use rate and half life data are in the same format
  if ((class(half_life_data) == "numeric" & !(is.na(usage_to_npc_output$iso3))) ||
    (class(half_life_data) != "numeric" & is.na(usage_to_npc_output$iso3))) {
    stop("Use rate and half lives need to be either both country-specific
    datasets (with ISO3) or both non-country-specific single values/vectors.")
  }

  if (class(half_life_data) == "numeric") {
    half_life_data <- data.frame(iso3 = NA, half_life = half_life_data)
  }

  # Merge net half lives with target nets per capita by country and target usage
  nets_distributed <- merge(
    x = usage_to_npc_output, y = half_life_data[, c("iso3", "half_life")],
    by = "iso3", all.x = TRUE, all.y = FALSE
  )
  nets_distributed$distribution_freq <- distribution_freq

  # Use net loss function to calculate annual nets distributed per capita
  # for different countries and target usages (the annual number of nets
  # distributed to maintain the input equilibrium nets per capita for a given
  # distribution cycle).
  nets_distributed$annual_percapita_nets_distributed <- nets_distributed$target_percapita_nets *
    sapply(nets_distributed$half_life, function(x) {
      mean(net_loss_function(t = seq(0, distribution_freq, 1), half_life = x, k = k))
    })
  nets_distributed <- nets_distributed[, c(
    "iso3", "use_rate", "half_life", "distribution_freq", "target_use",
    "target_access", "target_percapita_nets",
    "annual_percapita_nets_distributed"
  )]
  
  # Fill in output for a target usage of 0 (giving 0 on every metric)
  nets_distributed[nets_distributed$target_use==0,c("target_access")] <- 0
  nets_distributed[nets_distributed$target_use==0,c("target_percapita_nets")] <- 0
  nets_distributed[nets_distributed$target_use==0,c("annual_percapita_nets_distributed")] <- 0
  
  return(nets_distributed[order(nets_distributed$iso3, nets_distributed$target_use), ])
}
