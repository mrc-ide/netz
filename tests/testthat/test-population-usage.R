test_that("exp_loss_equilibrium", {
  expect_equal(exp_loss_equilibrium(1, 1, 1), 1 -  exp(-1))
})

test_that("population_usage", {
  
  timesteps <- 10 * 365
  pu <- population_usage_t(
    timesteps = 1:timesteps,
    distribution = 1,
    distribution_timesteps = 1,
    mean_retention = 5 * 365
  )
  
  expect_equal(length(pu), timesteps)
  expect_equal(pu, net_loss_exp(0:(timesteps - 1), 5 * 365))
  
  dt <- c(1, 101, 201, 301)
  pu <- population_usage_t(
    timesteps = 1:timesteps,
    distribution = c(1, 1, 1, 1),
    distribution_timesteps = dt,
    mean_retention = 5 * 365
  )
  
  expect_equal(pu[dt], rep(1, length(dt)))
})