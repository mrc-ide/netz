test_that("get_halflife works", {
  expect_equal(
    get_halflife(),
    median(halflife$half_life)
  )
  expect_equal(
    get_halflife("NGA"),
    median(halflife[halflife$iso3c == "NGA", "half_life"])
  )
  expect_error(
    get_halflife("IND"),
    "ISO code not found"
  )
})

test_that("get_usage_rate works", {
  expect_equal(
    get_usage_rate(),
    median(usage_rate$usage_rate)
  )
  expect_equal(
    get_usage_rate("NGA"),
    median(usage_rate[usage_rate$iso3c == "NGA", "usage_rate"])
  )
  expect_error(
    get_usage_rate("IND"),
    "ISO code not found"
  )
})