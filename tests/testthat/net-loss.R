test_that("net loss functions work", {
  expect_equal(net_loss_exp(t = 1, mean_retention = 1), exp(-(1 / 1) * 1))

  l <- 1 / sqrt(1 - 20 / (20 - log(0.5)))
  expect_equal(net_loss_map(t = 1, k = 20, half_life = 1), exp(20 - 20 / (1 - (1 / l)^2)))
})

test_that("net loss errors work", {
  expect_error(net_loss_exp(t = 1, mean_retention = -1), "mean_retention must be > 0")
  expect_error(net_loss_map(t = 1, k = 20, half_life = -1), "half_life must be > 0")
})
