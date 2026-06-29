# Convert nets per capita to annual nets per capital delivered

Convert nets per capita to annual nets per capital delivered

## Usage

``` r
crop_to_distribution(
  crop,
  crop_timesteps,
  distribution_timesteps,
  net_loss_function = netz::net_loss_map,
  min_distribution = NULL,
  max_distribution = NULL,
  ...
)
```

## Arguments

- crop:

  Vector of nets per capita

- crop_timesteps:

  Timesteps of crop estimates

- distribution_timesteps:

  Timesteps of distributions

- net_loss_function:

  Option to choose between exponential net loss (net_loss_exp) or the
  smooth-compact net loss function from Bertozzi-Villa, Amelia, et al.
  Nature communications 12.1 (2021): 1-12 (net_loss_map). Default =
  net_loss_map.

- min_distribution:

  Vector of optional minimum bounds on distributions

- max_distribution:

  Vector of optional maximum bounds on distributions

- ...:

  additional arguments for the net_loss_function

## Value

Annual nets per capita delivered
