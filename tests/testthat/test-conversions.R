test_that("usage_to_access works", {
  expect_equal(usage_to_access(0.1, 1), 0.1)
  expect_equal(usage_to_access(0.1, 0.5), 0.2)
  expect_warning(usage_to_access(0.3, 0.01), "Target usage(s) cannot be achieved with input usage_rates - return NA", fixed = TRUE)
  expect_error(usage_to_access(-1), "usage must be between 0 and 1")
  expect_error(usage_to_access(2), "usage must be between 0 and 1")
})

test_that("access_to_usage works", {
  expect_equal(access_to_usage(0.1, 1), 0.1)
  expect_equal(access_to_usage(0.1, 0.5), 0.05)
  expect_error(access_to_usage(-1), "access must be between 0 and 1")
  expect_error(access_to_usage(2), "access must be between 0 and 1")
})

test_that("npc_to_anpcd works", {
  expect_equal(npc_to_anpcd(c(1, 0.5, 0), 365 * 3, 1000), c(1, 0.5, 0) * mean(net_loss_map(0:(365*3), 1000)))
  expect_equal(npc_to_anpcd(c(1, 0.5, 0), 365 * 3, 1000, net_loss_exp), c(1, 0.5, 0) * mean(net_loss_exp(0:(365*3), 1000)))
  
  expect_error(npc_to_anpcd(-1, 365 * 3, 1000), "npc must be > 0")
  expect_error(npc_to_anpcd(0.5, -1, 1000), "distribution_freq must be > 0")
  expect_error(npc_to_anpcd(0.5, 200, -1), "half_life must be > 0")
})

test_that("anpcd_to_npc works", {
  expect_equal(anpcd_to_npc(c(1, 0.5, 0), 365 * 3, 1000), c(1, 0.5, 0) / mean(net_loss_map(0:(365*3), 1000)))
  expect_equal(anpcd_to_npc(c(1, 0.5, 0), 365 * 3, 1000, net_loss_exp), c(1, 0.5, 0) / mean(net_loss_exp(0:(365*3), 1000)))
  
  expect_error(anpcd_to_npc(-1, 365 * 3, 1000), "anpcd must be > 0")
  expect_error(anpcd_to_npc(0.5, -1, 1000), "distribution_freq must be > 0")
  expect_error(anpcd_to_npc(0.5, 200, -1), "half_life must be > 0")
})

test_that("access_to_npc works", {
  expect_equal(access_to_npc(1), NA_real_)
  expect_equal(access_to_npc(0), NA_real_)
  
  expect_error(access_to_npc(-1), "access must be between 0 and 1")
  expect_error(access_to_npc(2), "access must be between 0 and 1")
  
  expect_error(access_to_npc(0.5, "wrong"), "type must be one of: loess, loess_extrapolate or linear")
})

test_that("npc_to_access works", {
  expect_error(npc_to_access(0.5, "wrong"), "type must be one of: loess, loess_extrapolate or linear")
})