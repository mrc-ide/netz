# Estimate the population-level bed net usage at given times for a set of bed net distributions at given times. This assumes a specified rate of net loss and that recipients of multiple rounds are random.

Estimate the population-level bed net usage at given times for a set of
bed net distributions at given times. This assumes a specified rate of
net loss and that recipients of multiple rounds are random.

## Usage

``` r
model_distribution_to_usage(
  usage_timesteps,
  distribution,
  distribution_timesteps,
  net_loss_function = net_loss_exp,
  ...
)
```

## Arguments

- usage_timesteps:

  Target usage time points

- distribution:

  Vector of model distribution (% of total population)

- distribution_timesteps:

  A vector of distribution time steps

- net_loss_function:

  Function to calculate net retention over time. Should take time as
  first argument and return proportion retained.

- ...:

  Additional arguments passed to net_loss_function

## Value

Usage estimates at timepoints
