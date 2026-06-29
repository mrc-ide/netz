# Predict the access nets associated with a given nets per capita

Predict the access nets associated with a given nets per capita

## Usage

``` r
crop_to_access(crop, type = "loess", people_per_net = 1.66856)
```

## Arguments

- crop:

  A single or vector of nets per capita

- type:

  The npc to access model to use. This may be:

  "loess"

  :   : a loess fit to observed access-npc data

  "linear"

  :   : a linear fit to the observed access-npc data, fitted to the
      trend for observeation with access \< 0.5

- people_per_net:

  Assumed number of people who can use 1 net if type = "linear"

## Value

Predicted access
