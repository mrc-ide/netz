# Predict the nets per capita associated witha given access

Predict the nets per capita associated witha given access

## Usage

``` r
access_to_crop(access, type = "loess", people_per_net = 1.66856)
```

## Arguments

- access:

  A single or vector of access values

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

Predicted nets per capita
