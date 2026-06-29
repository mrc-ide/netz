# Get half life

Get half life

## Usage

``` r
get_halflife(iso3c, not_found_fn = stats::median, ...)
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

Half life
