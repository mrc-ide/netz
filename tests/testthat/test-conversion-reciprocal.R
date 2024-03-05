test_that("access_to_crop crop_to_access", {
  expect_equal(crop_to_access(access_to_crop(c(0.2, 0.5, 0.7))), c(0.2, 0.5, 0.7))
  expect_equal(access_to_crop(crop_to_access(c(0.2, 0.5, 0.6))), c(0.2, 0.5, 0.6), tolerance = 0.001)
  
  expect_equal(crop_to_access(access_to_crop(c(0, 0.5, 1), type = "loess"), type = "loess"), c(0, 0.5, 1))
  expect_equal(access_to_crop(crop_to_access(c(0.2, 0.5, 0.6), type = "loess"), type = "loess"), c(0.2, 0.5, 0.6), tolerance = 0.001)
  
  expect_equal(crop_to_access(access_to_crop(c(0, 0.2, 0.9), type = "linear"), type = "linear"), c(0, 0.2, 0.9))
  expect_equal(access_to_crop(crop_to_access(c(0, 0.2, 0.5), type = "linear"), type = "linear"), c(0, 0.2, 0.5))
})

test_that("usage_to_access access_to_usage", {
  expect_equal(access_to_usage(usage_to_access(c(0.2, 0.5, 0.7), 0.8), 0.8), c(0.2, 0.5, 0.7))
  expect_equal(usage_to_access(access_to_usage(c(0.2, 0.5, 0.7), 0.8), 0.8), c(0.2, 0.5, 0.7))
})

test_that("crop_to_distribution distribution_to_crop", {
  
  distribution_timesteps <- c(1, 365, 730)
  crop_timesteps <- distribution_timesteps + 180
  half_life <- 1000 
  
  crop <- c(0, 0.1, 0.5)
  expect_equal(
    distribution_to_crop(
      distribution =  crop_to_distribution(
        crop = crop,
        crop_timesteps = crop_timesteps,
        distribution_timesteps = distribution_timesteps,
        half_life = half_life),
      distribution_timesteps = distribution_timesteps,
      crop_timesteps = crop_timesteps,
      half_life = half_life
    ),
    crop
  )
  
  crop <- c(0, 0, 0)
  expect_equal(
    distribution_to_crop(
      distribution =  crop_to_distribution(
        crop = crop,
        crop_timesteps = crop_timesteps,
        distribution_timesteps = distribution_timesteps,
        half_life = half_life),
      distribution_timesteps = distribution_timesteps,
      crop_timesteps = crop_timesteps,
      half_life = half_life
    ),
    crop
  )
  
  crop <- c(1, 1, 1)
  expect_equal(
    distribution_to_crop(
      distribution =  crop_to_distribution(
        crop = crop,
        crop_timesteps = crop_timesteps,
        distribution_timesteps = distribution_timesteps,
        half_life = half_life),
      distribution_timesteps = distribution_timesteps,
      crop_timesteps = crop_timesteps,
      half_life = half_life
    ),
    crop
  )
  
  distribution <- c(0.1, 0.9, 0.1)
  expect_equal(
    crop_to_distribution(
      crop =  distribution_to_crop(
        distribution = distribution,
        distribution_timesteps = distribution_timesteps,
        crop_timesteps = crop_timesteps,
        half_life = half_life),
      crop_timesteps = crop_timesteps,
      distribution_timesteps = distribution_timesteps,
      half_life = half_life
    ),
    distribution
  )
  
  distribution <- c(0, 0, 0)
  expect_equal(
    crop_to_distribution(
      crop =  distribution_to_crop(
        distribution = distribution,
        distribution_timesteps = distribution_timesteps,
        crop_timesteps = crop_timesteps,
        half_life = half_life),
      crop_timesteps = crop_timesteps,
      distribution_timesteps = distribution_timesteps,
      half_life = half_life
    ),
    distribution
  )
  
  distribution <- c(1, 1, 1)
  expect_equal(
    crop_to_distribution(
      crop =  distribution_to_crop(
        distribution = distribution,
        distribution_timesteps = distribution_timesteps,
        crop_timesteps = crop_timesteps,
        half_life = half_life),
      crop_timesteps = crop_timesteps,
      distribution_timesteps = distribution_timesteps,
      half_life = half_life
    ),
    distribution
  )

})