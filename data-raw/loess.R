devtools::load_all()


# Pull in data on access and nets per capita
cube_nat_level <- get_npc_data()[, c("access_mean", "percapita_nets_mean")]

# Add 0,0 point at lower end
# Add point for access = 1, based on linear extrapolation of the top 5% of observed access data
cube_nat_level_top <- dplyr::filter(cube_nat_level, access_mean > quantile(access_mean, 0.95))
npc_pred <- predict(lm(percapita_nets_mean ~ access_mean, data = cube_nat_level_top), newdata = data.frame(access_mean = 1))
cube_nat_level_extrapolate <- dplyr::bind_rows(cube_nat_level, 
                                               data.frame(access_mean = c(0, 1),
                                                          percapita_nets_mean = c(0, npc_pred)))

# Isolate lower part of curve for a linear fit
cube_nat_level_linear <- cube_nat_level_extrapolate %>%
  dplyr::filter(access_mean < 0.5)

# Create loess fits object
npc_fits <- list(
  loess = loess(percapita_nets_mean ~ access_mean, data = cube_nat_level_extrapolate),
  linear = lm(percapita_nets_mean ~ 0 +  access_mean, data = cube_nat_level_linear)
)

# Check fits
access = seq(0, 1, 0.01)
plot(cube_nat_level_extrapolate)
lines(predict(npc_fits$loess, newdata = data.frame(access_mean = access)) ~ access, col =  "red")
lines(predict(npc_fits$linear, newdata = data.frame(access_mean = access)) ~ access, col =  "orange")

usethis::use_data(npc_fits, overwrite = TRUE)
