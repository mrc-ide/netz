test_that("access_to_crop crop_to_access", {
  expect_equal(crop_to_access(access_to_crop(c(0.2, 0.5, 0.7))), c(0.2, 0.5, 0.7))
  expect_equal(access_to_crop(crop_to_access(c(0.2, 0.5, 0.6))), c(0.2, 0.5, 0.6), tolerance = 0.001)
  
  expect_equal(crop_to_access(access_to_crop(c(0, 0.5, 1), type = "loess_extrapolate"), type = "loess_extrapolate"), c(0, 0.5, 1))
  expect_equal(access_to_crop(crop_to_access(c(0.2, 0.5, 0.6), type = "loess_extrapolate"), type = "loess_extrapolate"), c(0.2, 0.5, 0.6), tolerance = 0.001)
  
  expect_equal(crop_to_access(access_to_crop(c(0, 0.2, 0.9), type = "linear"), type = "linear"), c(0, 0.2, 0.9))
  expect_equal(access_to_crop(crop_to_access(c(0, 0.2, 0.5), type = "linear"), type = "linear"), c(0, 0.2, 0.5))
})

test_that("usage_to_access access_to_usage", {
  expect_equal(access_to_usage(usage_to_access(c(0.2, 0.5, 0.7), 0.8), 0.8), c(0.2, 0.5, 0.7))
  expect_equal(usage_to_access(access_to_usage(c(0.2, 0.5, 0.7), 0.8), 0.8), c(0.2, 0.5, 0.7))
})

test_that("crop_to_distribution distribution_to_crop", {
  expect_equal(distribution_to_crop(crop_to_distribution(c(0, 0.1, 0.5), 365 * 3, 1000), 365 * 3, 1000), c(0, 0.1, 0.5))
  expect_equal(distribution_to_crop(crop_to_distribution(c(0, 0.1, 0.5), 365 * 3, 1000, net_loss_exp), 365 * 3, 1000, net_loss_exp), c(0, 0.1, 0.5))
  
  expect_equal(crop_to_distribution(distribution_to_crop(c(0, 0.1, 0.5), 365 * 3, 1000), 365 * 3, 1000), c(0, 0.1, 0.5))
  expect_equal(crop_to_distribution(distribution_to_crop(c(0, 0.1, 0.5), 365 * 3, 1000, net_loss_exp), 365 * 3, 1000, net_loss_exp), c(0, 0.1, 0.5))
  
})