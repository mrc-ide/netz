# Function of net loss over time.

Net loss follows an S-shaped smooth compact curve, from Bertozzi-Villa,
Amelia, et al. Nature communications 12.1 (2021): 1-12.

## Usage

``` r
net_loss_map(t, half_life, k = 20)
```

## Arguments

- t:

  Single value or vector of timesteps in days.

- half_life:

  (Country-specific) half-life of nets in days.

- k:

  Fixed rate. Default = 20 from Bertozzi-Villa, Amelia, et al. Nature
  communications 12.1 (2021): 1-12.

## Value

The proportion of nets retained over time.
