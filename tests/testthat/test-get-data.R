test_that("get_halflife works", {
  expect_equal(
    get_halflife(),
    median(halflife_data$half_life)
  )
  expect_equal(
    get_halflife("NGA"),
    median(halflife_data[halflife_data$iso3c == "NGA", "half_life"])
  )
  expect_error(
    get_halflife("IND"),
    "ISO code not found"
  )
})

test_that("get_usage_rate works", {
  expect_equal(
    get_usage_rate(),
    median(usage_rate_data$usage_rate)
  )
  expect_equal(
    get_usage_rate("NGA"),
    median(usage_rate_data[usage_rate_data$iso3c == "NGA", "usage_rate"])
  )
  expect_error(
    get_usage_rate("IND"),
    "ISO code not found"
  )
})