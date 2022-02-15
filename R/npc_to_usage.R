#' Convert annual nets distributed per capita to estimated usage
#'
#' Calculates the estimated net usage from the annual number of nets distributed
#' per capita. Does the reverse of usage_to_annual_nets_distributed().
#'
#' Nets distributed is converted to nets per capita using half life data of
#' nets, which accounts for net loss over time within a distribution cycle. Nets
#' per capita is then converted to access using the observed relationship
#' between access and nets per capita across Africa, as described in
#' Bertozzi-Villa, Amelia, et al. Nature communications 12.1 (2021): 1-12. Then
#' access is converted to usage using the input use rate(s).
#'
#' @param target_nets A single value or vector of
#' desired target annual per capita net distributions to model.
#' @param distribution_freq A single distribution frequency of nets in days.
#' Default = 3*365 (3 years).
#' @param use_rate_data Dataset of country-specific net use rates obtained with
#' prepare_data() or a value/vector of net use rates.
#' Default = prepare_data()$use_rate_by_country. Can also be limited to specific
#' ISO3 codes.
#' @param half_life_data Dataset of country-specific net half lives obtained
#' with prepare_data() or a value/vector of net half lives (median retention
#' times) in days. Can also be limited to specific ISO3 codes.
#' @param access_vs_npc_data Dataset of net access against nets per capita
#' (Loess curve) obtained with prepare_data().
#' Default = prepare_data()$loess_for_prediction. Needs to include all
#' countries/ISO3 codes.
#' @param extrapolate_npc Option to extrapolate target nets per capita beyond
#' fitted Loess curve. Default = "loess" for continuation of Loess curve trend.
#' Use option "linear" for linear extrapolation. For any other inputs, NA
#' outputs are returned for any target usages exceeding currently observed
#' levels.
#' @param net_loss_function Option to choose between exponential net loss
#' (net_loss_exp) or the smooth-compact net loss function from Bertozzi-Villa,
#' Amelia, et al. Nature communications 12.1 (2021): 1-12 (net_loss_map).
#' Default = net_loss_map.
#' @param k Fixed rate parameter of the MAP function of net
#' loss (net_loss_map()), used to estimate the median net retention times by
#' country. Default = 20 from Bertozzi-Villa, Amelia, et al. Nature
#' communications 12.1 (2021): 1-12.
#'
#' @return Dataframe of usage, access, and nets per capita for the desired
#' input nets distributed by country.
#'
#' @importFrom stats 'approx' 'reshape' 'loess' 'predict' 'loess.control'
#' @export
convert_nets_to_usage <- function(target_nets,
                                   distribution_freq = 3 * 365,
                                   use_rate_data = prepare_data()$use_rate_by_country,
                                   half_life_data = prepare_data()$half_life_data,
                                   access_vs_npc_data = prepare_data()$loess_for_prediction,
                                   extrapolate_npc = "loess",
                                   net_loss_function = net_loss_map,
                                   k = 20) {
  if (class(target_nets) == "numeric" &
    class(use_rate_data) == "numeric" &
    class(half_life_data) == "numeric") {
    stop("One or more of the following must contain country level data, in the
          form of dataframe with an iso3 column: target_annual_percapita_nets_distributed,
          use_rate_data, half_life_data.")
  }

  ### 1. Convert annual nets distributed per capita to equilibrium nets per capita ###
  npc <- convert_nets_to_npc(
    nets_distributed = target_nets,
    distribution_freq = distribution_freq,
    half_life_data = half_life_data,
    use_rate_data = use_rate_data,
    net_loss_function = net_loss_function,
    k = k
  )

  ### 2. Convert nets per capita to usage ###
  usage <- convert_npc_to_usage(
    nets_to_npc_output = npc,
    use_rate_data = use_rate_data,
    access_vs_npc_data = access_vs_npc_data,
    extrapolate_npc = extrapolate_npc
  )
  return(usage)
}


#' Convert annual nets distributed per capita to equilibrium nets per capita ###
#'
#' Nets distributed is converted to nets per capita using half life data of
#' nets, which accounts for net loss over time within a distribution cycle.
#'
#' @param nets_distibuted Dataframe of annual nets per capita by country
#' based on different target nets distributed.
#' @inheritParams convert_nets_to_usage
#'
#' @return Dataframe of target nets distributed and estimated mean nets per
#' capita by country using the distribution frequency and country-specific net
#' loss functions.
#' @export
convert_nets_to_npc <- function(nets_distributed,
                                 distribution_freq = 3 * 365,
                                 half_life_data = prepare_data()$half_life_data,
                                 use_rate_data = prepare_data()$use_rate_by_country,
                                 net_loss_function = net_loss_map,
                                 k = 20) {

  ### 1. Convert annual nets distributed per capita to equilibrium nets per capita ###

  # If statements related to type of data.
  # If we do not have iso3 level data for nets distributed but we do for half
  # lives, expand the data frame of nets to include all iso3 codes we have data
  # for.
  if (!("iso3" %in% names(nets_distributed)) & (class(nets_distributed) == "numeric") &
    ("iso3" %in% names(half_life_data)) & (class(half_life_data) != "numeric")) {
    nets_distributed <- data.frame(
      iso3 = rep(half_life_data$iso3, length(nets_distributed)),
      annual_percapita_nets_distributed = sort(rep(nets_distributed, length(half_life_data$iso3)))
    )
    # If we have iso3 level data for nets distributed but we do not for half
    # lives, expand the data frame of half lives to include all iso3 codes we
    # have data for.
  } else if (("iso3" %in% names(nets_distributed)) & class(nets_distributed) != "numeric" &
    !("iso3" %in% names(half_life_data)) & (class(half_life_data) == "numeric")) {
    half_life_data <- data.frame(
      iso3 = rep(nets_distributed$iso3, length(half_life_data)),
      half_life = sort(rep(half_life_data, length(nets_distributed$iso3)))
    )
  } else if (("iso3" %in% names(use_rate_data)) & class(use_rate_data) != "numeric") {
    nets_distributed <- data.frame(
      iso3 = rep(use_rate_data$iso3, length(nets_distributed)),
      annual_percapita_nets_distributed = sort(rep(nets_distributed, length(use_rate_data$iso3)))
    )
    half_life_data <- data.frame(
      iso3 = rep(use_rate_data$iso3, length(half_life_data)),
      half_life = sort(rep(half_life_data, length(use_rate_data$iso3)))
    )
  } else {
    stop("One or more of the following must contain country level data, in the
          form of dataframe with an iso3 column: nets_distributed,
          use_rate_data, half_life_data.")
  }

  # Merge net half lives with target nets per capita by country and target usage
  output <- merge(
    x = nets_distributed, y = half_life_data[, c("iso3", "half_life")],
    by = "iso3", all.x = TRUE, all.y = FALSE
  )
  output$distribution_freq <- distribution_freq

  # Use net loss function to calculate mean nets per capita using the nets
  # distributed and the half lives of nets by country.
  output$mean_percapita_nets <- output$annual_percapita_nets_distributed /
    sapply(output$half_life, function(x) {
      mean(net_loss_function(t = seq(0, distribution_freq, 1), half_life = x, k = k))
    })
  output <- output[, c(
    "iso3", "half_life", "distribution_freq", "mean_percapita_nets",
    "annual_percapita_nets_distributed"
  )]

  return(output[order(output$iso3, output$annual_percapita_nets_distributed), ])
}


#' Convert nets per capita to bednet usage
#'
#' Calculates the net usage from the estimated nets per capita.
#' Nets per capita is converted to access using the observed relationship
#' between access and nets per capita across Africa, as described in
#' Bertozzi-Villa, Amelia, et al. Nature communications 12.1 (2021): 1-12.
#' The derived access is then converted to usage using the input use rate(s).
#'
#' @param nets_to_npc_output Output from
#' convert_annual_nets_distributed_to_npc(). Dataframe of target nets
#' distributed, and estimated mean nets per capita using the net loss function
#' and distribution frequency by country.
#' @inheritParams convert_nets_to_usage
#'
#' @return Dataframe of access, nets per capita and annual nets distributed per capita
#' for the desired steady-state target usage by country
#'
#' @export
#'
convert_npc_to_usage <- function(nets_to_npc_output,
                                 use_rate_data = prepare_data()$use_rate_by_country,
                                 access_vs_npc_data = prepare_data()$loess_for_prediction,
                                 extrapolate_npc = "loess") {

  ### 2. Convert nets per capita to usage ###

  if (class(use_rate_data) == "numeric") {
    use_rate_data <- data.frame(use_rate = rep(use_rate_data, nrow(nets_to_npc_output)))
  }

  if (extrapolate_npc == "linear") {
    # Add datapoint of (0,0) to allow linear extrapolation of nets per capita
    # for access levels beyond those observed
    loess_for_prediction <- rbind(
      access_vs_npc_data,
      data.frame(
        iso3 = NA, month = NA, access = 0,
        percapita_nets = 0, loess_predicted_access = 0
      )
    )
    print("Access-nets per capita curve is extrapolated linearly beyond observed levels")
  } else if (extrapolate_npc == "loess") {
    # extrapolated down to 0 nets = 0 access
    curve_fit <- loess(access ~ percapita_nets,
      data = access_vs_npc_data,
      control = loess.control(surface = "direct")
    )
    extrapolate_access <- data.frame(
      iso3 = NA, month = NA, access = NA, percapita_nets =
        seq(ceiling(min(access_vs_npc_data$percapita_nets) * 100) / 100, 0, -0.01)
    )
    extrapolate_access$loess_predicted_access <- predict(curve_fit, extrapolate_access$percapita_nets)
    loess_for_prediction <- rbind(access_vs_npc_data, extrapolate_access)
    print("Access-nets per capita curve is extrapolated beyond observed levels")
  } else {
    loess_for_prediction <- access_vs_npc_data
    print("No extrapolation beyond observed access-nets per capita relationship - return NA")
  }
  
  # Depending on whether given use rate is a single value or country specific,
  # use different methods to combine it with nets_to_npc_output.
  if(ncol(use_rate_data)>1){
    pred_usage <- merge(nets_to_npc_output, use_rate_data)
  } else if(ncol(use_rate_data)==1){
    pred_usage <- data.frame(nets_to_npc_output, 
                             use_rate = use_rate_data$use_rate)
  }

  loess_for_prediction <- loess_for_prediction[order(loess_for_prediction$iso3, loess_for_prediction$month), ]

  # Predict access using the observed relationship between nets per capita and
  # access, using the mean nets per capita calculated in the previous step.
  pred_usage$pred_access[pred_usage$mean_percapita_nets <= max(loess_for_prediction$percapita_nets)] <-
    approx(
      loess_for_prediction$percapita_nets,
      loess_for_prediction$loess_predicted_access,
      pred_usage$mean_percapita_nets[pred_usage$mean_percapita_nets <= max(loess_for_prediction$percapita_nets)]
    )$y

  # Convert target usage to target access: access = usage/use rate
  # since use rate = the proportion of people with access to a net who slept under it
  pred_usage$pred_use <- pred_usage$pred_access * pred_usage$use_rate
  pred_usage <- pred_usage[order(pred_usage$pred_use), ]

  # Relationship between access and NPC is aggregated for all countries,
  # but use rate to convert usage to access is country-specific (derived from spatiotemporal regression?)
  pred_usage <- pred_usage[order(pred_usage$iso3), ]

  pred_usage$pred_access[pred_usage$pred_access > 1] <- NA
  # Set access to NA where it is over 1 and excluded from conversion

  return(pred_usage)
}

