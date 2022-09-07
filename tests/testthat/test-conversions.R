test_that("usage_to_access works", {
  expect_equal(usage_to_access(0.1, 1), 0.1)
  expect_equal(usage_to_access(0.1, 0.5), 0.2)
  
  # Increasing the usage rate will decrease access
  expect_gt(usage_to_access(0.1, 0.5), usage_to_access(0.1, 1))
  
  expect_warning(usage_to_access(0.3, 0.01), "Target usage(s) cannot be achieved with input usage_rates - return NA", fixed = TRUE)
  expect_error(usage_to_access(-1), "usage must be between 0 and 1")
  expect_error(usage_to_access(2), "usage must be between 0 and 1")
})

test_that("access_to_usage works", {
  expect_equal(access_to_usage(0.1, 1), 0.1)
  expect_equal(access_to_usage(0.1, 0.5), 0.05)
  
  
  # Increasing the usage rate will increase usage
  expect_gt(access_to_usage(0.1, 1), access_to_usage(0.1, 0.5))
  
  expect_error(access_to_usage(-1), "access must be between 0 and 1")
  expect_error(access_to_usage(2), "access must be between 0 and 1")
})

test_that("crop_to_distribution works", {
  distribution_frequency <- 365 * 3
  half_life <- 1000
  
  nl <- net_loss_map(0:(100*365), 1000)
  index <- 1:length(nl) %% distribution_frequency
  snl <- mean(tapply(nl, index, sum))
  
  crop <- c(1, 0.5, 0)
  expect_equal(crop_to_distribution(crop, distribution_frequency, half_life), crop / ((distribution_frequency / 365) * snl))
  
  nl <- net_loss_exp(0:(100*365), 1000)
  index <- 1:length(nl) %% distribution_frequency
  snl <- mean(tapply(nl, index, sum))
  
  expect_equal(crop_to_distribution(crop, distribution_frequency, half_life, net_loss_exp), crop / ((distribution_frequency / 365) * snl))
  
  # Increasing half life should decrease required distribution
  expect_gt(crop_to_distribution(1, distribution_frequency, half_life, net_loss_exp),
            crop_to_distribution(1, distribution_frequency, half_life * 2, net_loss_exp))
  
  expect_error(crop_to_distribution(-1, 365 * 3, 1000), "crop must be > 0")
  expect_error(crop_to_distribution(0.5, -1, 1000), "distribution_freq must be > 0")
  expect_error(crop_to_distribution(0.5, 200, -1), "half_life must be > 0")
})

test_that("distribution_to_crop works", {
  distribution_frequency <- 365 * 3
  half_life <- 1000
  
  nl <- net_loss_map(0:(100*365), 1000)
  index <- 1:length(nl) %% distribution_frequency
  snl <- mean(tapply(nl, index, sum))
  
  dist <- c(1, 0.5, 0)
  expect_equal(distribution_to_crop(dist, distribution_frequency, half_life), dist * ((distribution_frequency / 365) * snl))
  
  nl <- net_loss_exp(0:(100*365), 1000)
  index <- 1:length(nl) %% distribution_frequency
  snl <- mean(tapply(nl, index, sum))
  
  expect_equal(distribution_to_crop(dist, distribution_frequency, half_life, net_loss_exp), dist * ((distribution_frequency / 365) * snl))
  
  # Increasing half life should increase resulting crop 
  expect_gt(distribution_to_crop(1, distribution_frequency, half_life * 2, net_loss_exp),
            distribution_to_crop(1, distribution_frequency, half_life , net_loss_exp))
  
  expect_error(distribution_to_crop(-1, 365 * 3, 1000), "distribution must be > 0")
  expect_error(distribution_to_crop(0.5, -1, 1000), "distribution_freq must be > 0")
  expect_error(distribution_to_crop(0.5, 200, -1), "half_life must be > 0")
})

test_that("access_to_crop works", {
  expect_equal(access_to_crop(1), NA_real_)
  expect_equal(access_to_crop(0), NA_real_)
  
  expect_error(access_to_crop(-1), "access must be between 0 and 1")
  expect_error(access_to_crop(2), "access must be between 0 and 1")
  
  expect_error(access_to_crop(0.5, "wrong"), "type must be one of: loess, loess_extrapolate or linear")
})

test_that("crop_to_access works", {
  expect_error(crop_to_access(0.5, "wrong"), "type must be one of: loess, loess_extrapolate or linear")
})