test_that("malariasimulation usage fitting works", {
  tu <- 0
  tut <- 365
  dt <- 365
  
  expect_equal(usage_to_model_distribution(0, tut, dt), 0)
  expect_equal(usage_to_model_distribution(1, tut, dt), 1)
  expect_equal(usage_to_model_distribution(c(1,1), (1:2) * 365, (1:2) * 365), c(1, 1))
  expect_equal(usage_to_model_distribution(c(0,0), (1:2) * 365, (1:2) * 365), c(0, 0))
  expect_equal(usage_to_model_distribution(c(1,0), (1:2) * 365, (1:2) * 365), c(1, 0))
  expect_equal(usage_to_model_distribution(c(1,1), (1:2) * 365, (1:3) * 365), c(1, 1, NA))
  
  expect_error(usage_to_model_distribution(tu, tut, dt, distribution_lower = -1),
               "All distribution lower values must be between 0 and 1")
  expect_error(usage_to_model_distribution(tu, tut, dt, distribution_lower = 6), 
               "All distribution lower values must be between 0 and 1")
  expect_error(usage_to_model_distribution(tu, tut, dt, distribution_upper = -1),
               "All distribution upper values must be between 0 and 1")
  expect_error(usage_to_model_distribution(tu, tut, dt, distribution_upper = 6),
               "All distribution upper values must be between 0 and 1")
  
  
  expect_error(usage_to_model_distribution(tu, tut, dt, distribution_lower = c(0.1, 0.1)),
               "distribution_timesteps, distribution_lower and distribution_upper must have equal lengths")
  expect_error(usage_to_model_distribution(tu, tut, dt, distribution_upper = c(0.1, 0.1)),
               "distribution_timesteps, distribution_lower and distribution_upper must have equal lengths")
  
  expect_error(usage_to_model_distribution(tu, c(tut, tut), dt),
               "Usage and usage timesteps must be the same length")
  
})


test_that("population_usage", {
  
  timesteps <- 10 * 365
  pu <- model_distribution_to_usage(
    usage_timesteps = 1:timesteps,
    distribution = 1,
    distribution_timesteps = 1,
    mean_retention = 5 * 365
  )
  
  expect_equal(length(pu), timesteps)
  expect_equal(pu, net_loss_exp(0:(timesteps - 1), 5 * 365))
  
  dt <- c(1, 101, 201, 301)
  pu <- model_distribution_to_usage(
    usage_timesteps = 1:timesteps,
    distribution = c(1, 1, 1, 1),
    distribution_timesteps = dt,
    mean_retention = 5 * 365
  )
  
  expect_equal(pu[dt], rep(1, length(dt)))
})