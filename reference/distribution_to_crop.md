# Convert annual nets per capital delivered to nets per capita

Convert annual nets per capital delivered to nets per capita

## Usage

``` r
distribution_to_crop(
  distribution,
  distribution_timesteps,
  crop_timesteps,
  net_loss_function = netz::net_loss_map,
  ...
)
```

## Arguments

- distribution:

  Nets per capital delivered

- distribution_timesteps:

  Timesteps of distributions

- crop_timesteps:

  Timesteps to return crop estimates

- net_loss_function:

  Option to choose between exponential net loss (net_loss_exp) or the
  smooth-compact net loss function from Bertozzi-Villa, Amelia, et al.
  Nature communications 12.1 (2021): 1-12 (net_loss_map). Default =
  net_loss_map.

- ...:

  additional arguments for the net_loss_function

## Value

Nets per capita
