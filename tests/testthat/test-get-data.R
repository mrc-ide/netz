test_that("get_halflife works", {
  expect_equal(
    get_halflife("No"),
    median(halflife$half_life)
  )
  expect_equal(
    get_halflife("No", mean),
    mean(halflife$half_life)
  )
  expect_equal(
    get_halflife("No", mean, trim = 0.2),
    mean(halflife$half_life, trim = 0.2)
  )
  expect_equal(
    get_halflife("NGA"),
    halflife[halflife$iso3c == "NGA", "half_life"]
  )
  expect_error(
    get_halflife()
  )
})

test_that("get_usage_rate works", {
  expect_equal(
    get_usage_rate("No"),
    median(usage_rate$usage_rate)
  )
  expect_equal(
    get_usage_rate("No", mean),
    mean(usage_rate$usage_rate)
  )
  expect_equal(
    get_usage_rate("No", mean, trim = 0.2),
    mean(usage_rate$usage_rate, trim = 0.2)
  )
  expect_equal(
    get_usage_rate("NGA"),
    usage_rate[usage_rate$iso3c == "NGA", "usage_rate"]
  )
  expect_error(
    get_usage_rate()
  )
})