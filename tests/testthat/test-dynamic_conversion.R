test_that("Distribution estimation equilibrates correctly", {
  
  d1 <- distribution_to_crop_dynamic(distribution = rep(0.2, 10),
                                     net_loss_function = net_loss_map,
                                     half_life = 365 * 2)
  
  d2 <- distribution_to_crop(distribution = 0.2, distribution_freq = 365 *3,
                             net_loss_function = net_loss_map,
                             half_life = 365 * 2)
  
  expect_equal(round(tail(d1, 1), 4), round(d2, 4))
  
  d1 <- distribution_to_crop_dynamic(distribution = rep(0.02, 20),
                                     net_loss_function = net_loss_exp,
                                     half_life = 365 * 5)
  
  d2 <- distribution_to_crop(distribution = 0.02, distribution_freq = 365 * 3,
                             net_loss_function = net_loss_exp,
                             half_life = 365 * 5)
  
  expect_equal(round(tail(d1, 1), 2), round(d2, 2))
})

test_that("Crop estimation equilibrates correctly", {
  c1 <- crop_to_distribution_dynamic(crop = rep(0.2, 10),
                                     net_loss_function = net_loss_map,
                                     half_life = 365 * 2)
  
  c2 <- crop_to_distribution(crop = 0.2,  distribution_freq = 365 *3,
                             net_loss_function = net_loss_map,
                             half_life = 365 * 2)
  
  expect_equal(round(tail(c1, 1), 4), round(c2, 4))
  
  c1 <- crop_to_distribution_dynamic(crop = rep(0.2, 20),
                                     net_loss_function = net_loss_exp,
                                     half_life = 365 * 5)
  
  c2 <- crop_to_distribution(crop = 0.2,  distribution_freq = 365 *3,
                             net_loss_function = net_loss_exp,
                             half_life = 365 * 5)
  
  expect_equal(round(tail(c1, 1), 2), round(c2, 2))
})

test_that("Dynamic distribution respects half life", {

  d1 <- distribution_to_crop_dynamic(distribution = rep(0.2, 3),
                                     net_loss_function = net_loss_map,
                                     half_life = 365 * 2)
  d2 <- distribution_to_crop_dynamic(distribution = rep(0.2, 3),
                                     net_loss_function = net_loss_map,
                                     half_life = 365 * 4)
  
  expect_true(all(d2 > d1))
  
  c1 <- crop_to_distribution_dynamic(crop = rep(0.2, 3),
                                     net_loss_function = net_loss_map,
                                     half_life = 365 * 2)
  c2 <- crop_to_distribution_dynamic(crop = rep(0.2, 3),
                                     net_loss_function = net_loss_map,
                                     half_life = 365 * 4)
  expect_true(all(c1 > c2))
  
  d1 <- distribution_to_crop_dynamic(distribution = rep(0.2, 3),
                                     net_loss_function = net_loss_exp,
                                     half_life = 365 * 2)
  d2 <- distribution_to_crop_dynamic(distribution = rep(0.2, 3),
                                     net_loss_function = net_loss_exp,
                                     half_life = 365 * 4)
  
  expect_true(all(d2 > d1))
  
  c1 <- crop_to_distribution_dynamic(crop = rep(0.2, 3),
                                     net_loss_function = net_loss_exp,
                                     half_life = 365 * 2)
  c2 <- crop_to_distribution_dynamic(crop = rep(0.2, 3),
                                     net_loss_function = net_loss_exp,
                                     half_life = 365 * 4)
  expect_true(all(c1 > c2))
})

test_that("Test shifted", {
  l <- 1:10
  s1 <- shifted(dt = 10, l = l, t = 20)
  o1 <- c(rep(0, 10), l)
  expect_equal(s1, o1)
})
