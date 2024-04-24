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
  expect_equal(
    crop_to_distribution(
      crop = 1,
      crop_timesteps = 1,
      distribution_timesteps = 1,
      half_life = 700
    ),
    1
  )
  
  expect_equal(
    crop_to_distribution(
      crop = 0,
      crop_timesteps = 1,
      distribution_timesteps = 1,
      half_life = 700
    ),
    0
  )
  
  expect_equal(
    crop_to_distribution(
      crop = 1,
      crop_timesteps = 1,
      distribution_timesteps = 1,
      max_distribution = 0.5,
      half_life = 700
    ),
    0.5
  )
  
  expect_equal(
    crop_to_distribution(
      crop = 0,
      crop_timesteps = 1,
      distribution_timesteps = 1,
      min_distribution = 0.5,
      half_life = 700
    ),
    0.5
  )
  
  # Check behaviour if distirbution timestep is after last crop timestep
  expect_equal(
    crop_to_distribution(
      crop = 0.1,
      crop_timesteps = 1,
      distribution_timesteps = 2,
      half_life = 700
    ),
    NA
  )
  
  expect_error(
    crop_to_distribution(
      crop = 0,
      crop_timesteps = 1,
      distribution_timesteps = 1,
      min_distribution = c(0.5, 0.5),
      half_life = 700
    ),
    "min and max distribution vectors must be of length(distribution_timesteps)",
    fixed = TRUE
  )
  
  expect_error(
    crop_to_distribution(
      crop = 0,
      crop_timesteps = 1,
      distribution_timesteps = 1,
      max_distribution = c(0.5, 0.5),
      half_life = 700
    ),
    "min and max distribution vectors must be of length(distribution_timesteps)",
    fixed = TRUE
  )
})

test_that("distribution_to_crop works", {
  expect_equal(
    distribution_to_crop(
      distribution = 1,
      distribution_timesteps = 1,
      crop_timesteps = 1,
      half_life = 700
    ),
    1
  )
  
  expect_equal(
    distribution_to_crop(
      distribution = 0,
      distribution_timesteps = 1,
      crop_timesteps = 1,
      half_life = 700
    ),
    0
  )
})

test_that("access_to_crop works", {
  expect_equal(access_to_crop(0), 0)
  
  expect_error(access_to_crop(-1), "access must be between 0 and 1")
  expect_error(access_to_crop(2), "access must be between 0 and 1")
  
  expect_error(access_to_crop(0.5, "wrong"), "type must be one of: loess, linear")
})

test_that("crop_to_access works", {
  expect_error(crop_to_access(0.5, "wrong"), "type must be one of: loess, linear")
})