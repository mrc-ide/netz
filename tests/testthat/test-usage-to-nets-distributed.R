test_that("usage to npc conversion works", {
  usage_vec <- c(0.1, 0.5, 0.8)
  use_rates <- c(0.8, 0.9)
  access_vs_npc <- prepare_data()$loess_for_prediction
  access_vs_npc2 <- access_vs_npc
  access_vs_npc2$access[5] <- access_vs_npc2$access[5] * -1

  expect_error(
    convert_usage_to_npc(
      target_usage = 1.1,
      use_rate_data = use_rates,
      access_vs_npc_data = access_vs_npc,
      extrapolate_npc = "loess"
    ),
    "All target usage values must be between 0 and 1"
  )
  expect_error(
    convert_usage_to_npc(
      target_usage = -0.5,
      use_rate_data = use_rates,
      access_vs_npc_data = access_vs_npc,
      extrapolate_npc = "loess"
    ),
    "All target usage values must be between 0 and 1"
  )
  expect_error(
    convert_usage_to_npc(
      target_usage = usage_vec,
      use_rate_data = c(1.5, 0.9),
      access_vs_npc_data = access_vs_npc,
      extrapolate_npc = "loess"
    ),
    "All use rate values must be between 0 and 1"
  )
  expect_error(
    convert_usage_to_npc(
      target_usage = usage_vec,
      use_rate_data = use_rates,
      access_vs_npc_data = access_vs_npc2,
      extrapolate_npc = "loess"
    ),
    "All access values in access_vs_npc_data must be between 0 and 1"
  )
  expect_error(
    convert_usage_to_npc(
      target_usage = usage_vec,
      use_rate_data = use_rates,
      access_vs_npc_data = access_vs_npc,
      extrapolate_npc = "no"
    ),
    "extrapolate_npc must be one of: loess, linear, FALSE"
  )
  expect_output(
    convert_usage_to_npc(
      target_usage = usage_vec,
      use_rate_data = use_rates,
      access_vs_npc_data = access_vs_npc,
      extrapolate_npc = "loess"
    ),
    "Access-nets per capita curve is extrapolated beyond observed levels"
  )
  expect_error(expect_output(convert_usage_to_npc(
    target_usage = c(0.8, 0.9),
    use_rate_data = c(0.6, 0.7),
    access_vs_npc_data = access_vs_npc,
    extrapolate_npc = "loess"
  )),
  "Target usage(s) cannot be achieved with any of the input use rates. Refer to maximum usage above.",
  fixed = TRUE
  )
  expect_warning(expect_output(convert_usage_to_npc(
    target_usage = c(0.7, 0.8),
    use_rate_data = c(0.6, 0.7),
    access_vs_npc_data = access_vs_npc,
    extrapolate_npc = "loess"
  )),
  "Target usage(s) cannot be achieved with some of the input use rates - return NA. Refer to maximum usage above.",
  fixed = TRUE
  )

  res1 <- convert_usage_to_npc(
    target_usage = 1,
    use_rate_data = 1,
    access_vs_npc_data = access_vs_npc,
    extrapolate_npc = "linear"
  )
  expect_equal(res1$target_access, 1)
  expect_equal(res1$target_percapita_nets, 1)

  res2 <- suppressWarnings(convert_usage_to_npc(
    target_usage = c(0.8, 0.9),
    use_rate_data = 0.8,
    access_vs_npc_data = access_vs_npc,
    extrapolate_npc = "loess"
  ))
  expect_true(is.na(res2$target_percapita_nets[1]))
  expect_true(is.na(res2$target_percapita_nets[2]))
})

test_that("npc to nets distributed conversion works", {
  npc_out <- convert_usage_to_npc(
    target_usage = c(0.5, 0.6),
    use_rate_data = c(0.8),
    extrapolate_npc = "loess"
  )

  expect_error(convert_npc_to_annual_nets_distributed(
    usage_to_npc_output = npc_out,
    distribution_freq = c(365, 3 * 365),
    half_life_data = 1.64 * 365,
    net_loss_function = net_loss_map,
    k = 20
  ), "distribution_freq must be a single value (in days)", fixed = TRUE)
  
})

test_that("net loss functions work", {
  expect_equal(net_loss_exp(t = 1, half_life = 1), exp(-(1 / 1) * 1))

  l <- 1 / sqrt(1 - 20 / (20 - log(0.5)))
  expect_equal(net_loss_map(t = 1, k = 20, half_life = 1), exp(20 - 20 / (1 - (1 / l)^2)))
})
