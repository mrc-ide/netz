# Get usage rate

Get usage rate

## Usage

``` r
get_usage_rate(iso3c, not_found_fn = stats::median, ...)
```

## Arguments

- iso3c:

  ISO3c code.

- not_found_fn:

  A function to summarise across all available half-life estimates to
  produce an estimate for countries not in the dataset

- ...:

  Further arguments passed to not_found_fn

## Value

Usage rate
