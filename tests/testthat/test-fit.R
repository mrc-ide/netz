test_that("usage fitting works", {
  tu <- 0
  tut <- 365
  dt <- 365
  
  expect_error(fit_usage(tu, tut, dt, distribution_lower = -1),
               "All distribution lower values must be between 0 and 1")
  expect_error(fit_usage(tu, tut, dt, distribution_lower = 6), 
               "All distribution lower values must be between 0 and 1")
  expect_error(fit_usage(tu, tut, dt, distribution_upper = -1),
               "All distribution upper values must be between 0 and 1")
  expect_error(fit_usage(tu, tut, dt, distribution_upper = 6),
               "All distribution upper values must be between 0 and 1")
  
  
  expect_error(fit_usage(tu, tut, dt, distribution_lower = c(0.1, 0.1)),
               "distribution_timesteps, distribution_lower and distribution_upper must have equal lengths")
  expect_error(fit_usage(tu, tut, dt, distribution_upper = c(0.1, 0.1)),
               "distribution_timesteps, distribution_lower and distribution_upper must have equal lengths")
  
  expect_error(fit_usage(tu, c(tut, tut), dt),
               "Target usage and target usage timesteps must be the same length")
  
  expect_equal(
    fit_usage_objective(
      distribution = 0,
      distribution_timesteps = 1,
      target_usage = 0,
      target_usage_timesteps = 1,
      mean_retention = 365 * 5), 
    0
  )
  
  fit1 <- fit_usage(
    target_usage = 0,
    target_usage_timesteps = 1, 
    distribution_timesteps = 1,
    mean_retention = 365 * 5
  )
  expect_equal(fit1$par, 0)
  
})
