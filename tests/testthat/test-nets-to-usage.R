test_that("conversion from nets distributed to usage", {
  expect_error(convert_nets_to_usage(
    target_nets = seq(0, 1, 0.1),
    distribution_freq = 3 * 365,
    use_rate_data = 1.64,
    half_life_data = prepare_data()$half_life_data,
    access_vs_npc_data = prepare_data()$loess_for_prediction,
    extrapolate_npc = "loess",
    net_loss_function = net_loss_map,
    k = 20
  ))
  usage_data <- convert_nets_to_usage(
    target_nets = seq(0, 1, 0.1),
    distribution_freq = 180,
    use_rate_data = prepare_data()$use_rate_by_country,
    half_life_data = prepare_data()$half_life_data,
    access_vs_npc_data = prepare_data()$loess_for_prediction,
    extrapolate_npc = "loess",
    net_loss_function = net_loss_map,
    k = 20
  )
  expect_true(max(usage_data$pred_use) <= 1)
  expect_true(max(usage_data$pred_access) <= 1)
  expect_true(max(usage_data$use_rate) <= 1)
})

test_that("nets to npc", {
  nets <- seq(0, 2, 0.2)

  expect_error(
    convert_nets_to_npc(nets,
      distribution_freq = 3 * 365,
      half_life_data = 405,
      use_rate_data = 0.65,
      net_loss_function = net_loss_map,
      k = 20
    )
  )
  expect_silent(
    convert_nets_to_npc(nets,
      distribution_freq = 3 * 365,
      half_life_data = prepare_data()$half_life_data,
      use_rate_data = 0.65,
      net_loss_function = net_loss_map,
      k = 20
    )
  )
  expect_silent(
    convert_nets_to_npc(nets,
      distribution_freq = 3 * 365,
      half_life_data = 800,
      use_rate_data = prepare_data()$use_rate_by_country,
      net_loss_function = net_loss_exp,
      k = 20
    )
  )
})

test_that("npc to usage works", {
  nets <- seq(0, 1, 0.2)
  nets_to_npc_output <- convert_nets_to_npc(nets,
    distribution_freq = 3 * 365,
    half_life_data = prepare_data()$half_life_data,
    use_rate_data = prepare_data()$use_rate_by_country,
    net_loss_function = net_loss_exp,
    k = 20
  )

  usages <- convert_npc_to_usage(nets_to_npc_output,
    use_rate_data = prepare_data()$use_rate_by_country,
    access_vs_npc_data = prepare_data()$loess_for_prediction,
    extrapolate_npc = "linear"
  )
  usages <- usages[usages$annual_percapita_nets_distributed == 0, ]
  expect_equal(mean(usages$pred_access), 0)
  expect_equal(mean(usages$pred_use), 0)

  usages <- convert_npc_to_usage(nets_to_npc_output,
    use_rate_data = prepare_data()$use_rate_by_country,
    access_vs_npc_data = prepare_data()$loess_for_prediction,
    extrapolate_npc = "loess"
  )
  usages <- usages[usages$annual_percapita_nets_distributed == 0, ]
  expect_equal(mean(usages$mean_percapita_nets), 0)
})
