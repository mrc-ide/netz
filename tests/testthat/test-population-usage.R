test_that("exp_loss_equilibrium", {
  expect_equal(exp_loss_equilibrium(1, 1, 1), 1 -  exp(-1))
})

test_that("sample_intervention", {
  expect_equal(sample_intervention(0, 0, 0), FALSE)
  expect_equal(sample_intervention(1, 0, 0), TRUE)
  
  expect_warning(sample_intervention(-1, 0, 0), "NaNs produced")
  expect_warning(sample_intervention(2, 0, 0), "NaNs produced")
})

test_that("get_correlation", {
  cor <- get_correlation(10, 0)
  expect_equal(cor$mvnorm[,1], rep(0, 10))
  expect_equal(cor$sigma_squared[1], c(0))
  
  expect_error(get_correlation(10, -1), "rho for bednets must be between 0 and 1")
  expect_error(get_correlation(10, 2), "rho for bednets must be between 0 and 1")
})

test_that("population_usage", {
  net_retention_hl <- 5
  net_loss <- exp(- (1 / (net_retention_hl * 365)) * 1:10000)
  population = 100
  timesteps = 365 * 30
  
  set.seed(1)
  pu <- population_usage(
    net_loss = net_loss,
    population = population,
    timesteps = timesteps,
    distribution = rep(c(0.5, 0, 0), 10),
    distribution_timesteps = 365 * (1:30),
    rho = 0.99)
  
  expect_equal(length(pu), timesteps)
  expect_equal(sum(pu[1:364]), 0)
  expect_gt(pu[365], 0)
})