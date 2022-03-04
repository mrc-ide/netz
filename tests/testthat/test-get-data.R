test_that("get_halflife_data works", {
  d <- get_halflife_data()
  expect_type(d, "list")
  expect_equal(dim(d), c(40, 2))
  expect_named(d, c("iso3", "half_life"))
})

test_that("get_usage_rate_data works", {
  d <- get_usage_rate_data()
  expect_type(d, "list")
  expect_equal(dim(d), c(40, 2))
  expect_named(d, c("iso3", "usage_rate"))
})


test_that("get_npc_data works", {
  d <- get_npc_data()
  expect_type(d, "list")
  expect_equal(dim(d), c(480, 11))
  expect_named(d, c("iso3", "country_name", "year", "month", "time",
                    "access_mean", "access_lower", "access_upper",
                    "percapita_nets_mean",  "percapita_nets_lower",
                    "percapita_nets_upper"))
})
