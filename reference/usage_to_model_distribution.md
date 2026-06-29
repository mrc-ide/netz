# Estimate the malariasimulation input distribution from usage assumes a specified net loss function and randomly correlated net distribution.

Estimate the malariasimulation input distribution from usage assumes a
specified net loss function and randomly correlated net distribution.

## Usage

``` r
usage_to_model_distribution(
  usage,
  usage_timesteps,
  distribution_timesteps,
  distribution_lower = rep(0, length(distribution_timesteps)),
  distribution_upper = rep(1, length(distribution_timesteps)),
  net_loss_function = net_loss_exp,
  ...
)
```

## Arguments

- usage:

  Target usage

- usage_timesteps:

  Target usage time points

- distribution_timesteps:

  A vector of distribution time steps

- distribution_lower:

  Lower bound on distributions (default = 0)

- distribution_upper:

  Upper bound on distribution (default = 1)

- net_loss_function:

  Function to calculate net retention over time. Should take time as
  first argument and return proportion retained. Defaults to
  net_loss_exp for backward compatibility.

- ...:

  Additional arguments passed to net_loss_function

## Details

Note, the estimation works sequentially in time order, so if there are
multiple distribution steps specified between target usage timestep they
will be inferred sequentially.
