# Get Half life and net retention data


# estimates of net half lives (median retention times) in years
halflife <- utils::read.csv(paste0("https://raw.github.com/",
                                        "bertozzivill/map-itn-cube/",
                                        "publication-2021/paper_figures/",
                                        "figure_data/fig_5_llin_half_lives.csv"))

# Convert half lives into days
halflife <- halflife[, c("iso3", "half_life")]
colnames(halflife) <- c("iso3c", "half_life")
halflife$half_life <- halflife$half_life * 365


# estimates of net half lives (median retention times) in years
usage_rate <- utils::read.csv(paste0("https://raw.github.com/",
                                          "bertozzivill/map-itn-gts/",
                                          "master/data/coverage_metrics/",
                                          "aggregated_predictions_2019.csv"))

# Convert half lives into days
usage_rate <- usage_rate[usage_rate$variable == "use_rate", ]
usage_rate <- usage_rate[!usage_rate$iso3 == "AFR",]
usage_rate <- usage_rate[is.na(usage_rate$month),]
usage_rate <- usage_rate[, c("iso3", "mean")]
colnames(usage_rate) <- c("iso3c", "usage_rate")

# Crop data
crop_data <- utils::read.csv(paste0("https://raw.github.com/",
                                     "bertozzivill/map-itn-cube/",
                                     "publication-2021/paper_figures/",
                                     "figure_data/fig_4_access_npc.csv"))

usethis::use_data(halflife, overwrite = TRUE)
usethis::use_data(usage_rate, overwrite = TRUE)
usethis::use_data(crop_data, overwrite = TRUE)

