test_that("malariasimulation usage fitting works - exponential model", {
  tu <- 0
  tut <- 365
  dt <- 365

  expect_equal(
    usage_to_model_distribution(0, tut, dt, mean_retention = 5 * 365),
    0
  )
  expect_equal(
    usage_to_model_distribution(1, tut, dt, mean_retention = 5 * 365),
    1
  )
  expect_equal(
    usage_to_model_distribution(
      c(1, 1),
      (1:2) * 365,
      (1:2) * 365,
      mean_retention = 5 * 365
    ),
    c(1, 1)
  )
  expect_equal(
    usage_to_model_distribution(
      c(0, 0),
      (1:2) * 365,
      (1:2) * 365,
      mean_retention = 5 * 365
    ),
    c(0, 0)
  )
  expect_equal(
    usage_to_model_distribution(
      c(1, 0),
      (1:2) * 365,
      (1:2) * 365,
      mean_retention = 5 * 365
    ),
    c(1, 0)
  )
  expect_equal(
    usage_to_model_distribution(
      c(1, 1),
      (1:2) * 365,
      (1:3) * 365,
      mean_retention = 5 * 365
    ),
    c(1, 1, NA)
  )

  expect_error(
    usage_to_model_distribution(
      tu,
      tut,
      dt,
      distribution_lower = -1,
      mean_retention = 5 * 365
    ),
    "All distribution lower values must be between 0 and 1"
  )
  expect_error(
    usage_to_model_distribution(
      tu,
      tut,
      dt,
      distribution_lower = 6,
      mean_retention = 5 * 365
    ),
    "All distribution lower values must be between 0 and 1"
  )
  expect_error(
    usage_to_model_distribution(
      tu,
      tut,
      dt,
      distribution_upper = -1,
      mean_retention = 5 * 365
    ),
    "All distribution upper values must be between 0 and 1"
  )
  expect_error(
    usage_to_model_distribution(
      tu,
      tut,
      dt,
      distribution_upper = 6,
      mean_retention = 5 * 365
    ),
    "All distribution upper values must be between 0 and 1"
  )

  expect_error(
    usage_to_model_distribution(
      tu,
      tut,
      dt,
      distribution_lower = c(0.1, 0.1),
      mean_retention = 5 * 365
    ),
    "distribution timesteps, lower bounds and upper bounds must have same length"
  )
  expect_error(
    usage_to_model_distribution(
      tu,
      tut,
      dt,
      distribution_upper = c(0.1, 0.1),
      mean_retention = 5 * 365
    ),
    "distribution timesteps, lower bounds and upper bounds must have same length"
  )

  expect_error(
    usage_to_model_distribution(tu, c(tut, tut), dt, mean_retention = 5 * 365),
    "Usage and usage timesteps must be the same length"
  )
})

test_that("malariasimulation usage fitting works - MAP model", {
  tu <- 0
  tut <- 365
  dt <- 365

  expect_equal(
    usage_to_model_distribution(
      0,
      tut,
      dt,
      net_loss_function = net_loss_map,
      half_life = 2 * 365
    ),
    0
  )
  expect_equal(
    usage_to_model_distribution(
      1,
      tut,
      dt,
      net_loss_function = net_loss_map,
      half_life = 2 * 365
    ),
    1
  )
  expect_equal(
    usage_to_model_distribution(
      c(1, 1),
      (1:2) * 365,
      (1:2) * 365,
      net_loss_function = net_loss_map,
      half_life = 2 * 365
    ),
    c(1, 1)
  )
  expect_equal(
    usage_to_model_distribution(
      c(0, 0),
      (1:2) * 365,
      (1:2) * 365,
      net_loss_function = net_loss_map,
      half_life = 2 * 365
    ),
    c(0, 0)
  )
  expect_equal(
    usage_to_model_distribution(
      c(1, 0),
      (1:2) * 365,
      (1:2) * 365,
      net_loss_function = net_loss_map,
      half_life = 2 * 365
    ),
    c(1, 0)
  )
  expect_equal(
    usage_to_model_distribution(
      c(1, 1),
      (1:2) * 365,
      (1:3) * 365,
      net_loss_function = net_loss_map,
      half_life = 2 * 365
    ),
    c(1, 1, NA)
  )

  expect_error(
    usage_to_model_distribution(
      tu,
      tut,
      dt,
      distribution_lower = -1,
      net_loss_function = net_loss_map,
      half_life = 2 * 365
    ),
    "All distribution lower values must be between 0 and 1"
  )
  expect_error(
    usage_to_model_distribution(
      tu,
      tut,
      dt,
      distribution_lower = 6,
      net_loss_function = net_loss_map,
      half_life = 2 * 365
    ),
    "All distribution lower values must be between 0 and 1"
  )
  expect_error(
    usage_to_model_distribution(
      tu,
      tut,
      dt,
      distribution_upper = -1,
      net_loss_function = net_loss_map,
      half_life = 2 * 365
    ),
    "All distribution upper values must be between 0 and 1"
  )
  expect_error(
    usage_to_model_distribution(
      tu,
      tut,
      dt,
      distribution_upper = 6,
      net_loss_function = net_loss_map,
      half_life = 2 * 365
    ),
    "All distribution upper values must be between 0 and 1"
  )

  expect_error(
    usage_to_model_distribution(
      tu,
      tut,
      dt,
      distribution_lower = c(0.1, 0.1),
      net_loss_function = net_loss_map,
      half_life = 2 * 365
    ),
    "distribution timesteps, lower bounds and upper bounds must have same length"
  )
  expect_error(
    usage_to_model_distribution(
      tu,
      tut,
      dt,
      distribution_upper = c(0.1, 0.1),
      net_loss_function = net_loss_map,
      half_life = 2 * 365
    ),
    "distribution timesteps, lower bounds and upper bounds must have same length"
  )

  expect_error(
    usage_to_model_distribution(
      tu,
      c(tut, tut),
      dt,
      net_loss_function = net_loss_map,
      half_life = 2 * 365
    ),
    "Usage and usage timesteps must be the same length"
  )
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

test_that("zero distributions are dealt with correctly", {
  zero_dist <- model_distribution_to_usage(
    usage_timesteps = 1:3,
    distribution = c(0, 0),
    distribution_timesteps = c(1, 2),
    mean_retention = 5 * 365
  )
  expect_equal(zero_dist, c(0, 0, 0))
})

test_that("Input distribution estimation deals with 
  input timepoints after final target use timepoint elegantly", {
  expect_equal(
    usage_to_model_distribution(
      usage = 0.1,
      usage_timesteps = 1,
      distribution_timesteps = 1:3,
      distribution_lower = rep(0, 3),
      distribution_upper = rep(1, 3),
      net_loss_function = net_loss_map,
      half_life = 300
    ),
    c(0.1, NA, NA)
  )
})

test_that("Updated forward problem runs correctly", {
  net_retention <- function(t) net_loss_map(t, half_life = 600)
  d1 <- 0.5
  d2 <- 0.4
  expect_equal(
    model_distribution_to_usage(
      usage_timesteps = 500,
      distribution = c(d1, d2),
      distribution_timesteps = c(0, 365),
      net_loss_function = net_loss_map,
      half_life = 600
    ),
    d2 * net_retention(500 - 365) + (1 - d2) * d1 * net_retention(500)
  )
})


test_that("Updated inverse problem runs correctly", {
  net_retention <- function(t) net_loss_map(t, half_life = 300)
  u1 <- 0.4
  u2 <- 0.6

  d1 <- u1 / net_retention(100)
  previous_usage <- d1 * net_retention(300)
  d2 <- (u2 - previous_usage) / (net_retention(100) - previous_usage)

  expect_equal(
    usage_to_model_distribution(
      usage = c(u1, u2),
      usage_timesteps = c(100, 300),
      distribution_timesteps = c(0, 200),
      net_loss_function = net_loss_map,
      half_life = 300
    ),
    c(d1, d2)
  )
})


test_that("Usage to distribution to usage recovers initial usage (MAP)", {
  usage <- c(0.5, 0.6, 0.6, 0.7)
  distribution_timesteps <- c(1, 201, 401, 601)
  usage_timesteps <- distribution_timesteps + 100

  distribution <- usage_to_model_distribution(
    usage, usage_timesteps, distribution_timesteps,
    net_loss_function = net_loss_map, half_life = 200
  )
  expect_equal(
    model_distribution_to_usage(
      usage_timesteps, distribution, distribution_timesteps,
      net_loss_function = net_loss_map, half_life = 200
    ),
    usage
  )
})

test_that("Usage to distribution to usage recovers initial usage (exp)", {
  usage <- c(0.5, 0.6, 0.6, 0.7)
  distribution_timesteps <- c(1, 201, 401, 601)
  usage_timesteps <- distribution_timesteps + 100

  distribution <- usage_to_model_distribution(
    usage, usage_timesteps, distribution_timesteps,
    net_loss_function = net_loss_exp, mean_retention = 400
  )
  expect_equal(
    model_distribution_to_usage(
      usage_timesteps, distribution, distribution_timesteps,
      net_loss_function = net_loss_exp, mean_retention = 400
    ),
    usage
  )
})