## calculate_metrics ##
# calculate and save metrics

# input -------------------------------------------------------------------

ifile = 'data/processed/box_surveys.rds'
ofile = 'data/processed/metrics.rds'

# setup -------------------------------------------------------------------

library(tidyverse)

# process -----------------------------------------------------------------

df = readRDS(ifile)

# summarize
metrics = df %>%
  group_by(platform,n_whales,box_type) %>%
  summarize(
    platform = unique(platform),
    n_whales = unique(n_whales),
    behavior = unique(behavior),
    transits = length(unique(run)),
    transits_with_detections = sum(detected),
    transit_p = transits_with_detections/transits,
    mean_transit_time = mean(transit_time, na.rm = TRUE),
    mean_transit_dist = mean(transit_dist, na.rm = TRUE),
    mean_transit_area = mean(transit_area, na.rm = TRUE),
    mean_detections = mean(n_detected, na.rm = TRUE),
    det_per_hour = mean_detections/mean_transit_time*60*60, # per hour
    det_per_dist = mean_detections/mean_transit_dist,
    det_per_area = mean_detections/mean_transit_area, # per km squared per hour
    det_area_time = mean_detections/mean_transit_area/mean_transit_time*60*60,
    cost_per_hour = NA,
    .groups = 'drop'
  )

# add the cost per hour for every platform
metrics$cost_per_hour = NA
metrics$cost_per_hour[metrics$platform == 'plane'] = 1592
metrics$cost_per_hour[metrics$platform == 'vessel'] = 700
metrics$cost_per_hour[metrics$platform == 'slocum'] = 31.25

# calculate cost per detection
metrics$cost_per_det = metrics$cost_per_hour/metrics$det_per_hour

# delete cost per hour column
metrics = metrics %>% dplyr::select(-cost_per_hour)

# save
saveRDS(metrics, ofile)


