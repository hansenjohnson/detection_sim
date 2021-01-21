## plot_box_survey_results ##
# summarize and plot output from box survey simulation

# setup -------------------------------------------------------------------

library(tidyverse)
source('r/box_survey_functions.R')

# define colors for each platform
platform_cols = c('glider' = 'blue', 'plane' = 'red', 'vessel' = 'darkslategrey')

# process -----------------------------------------------------------------

# read in simulated survey data
df = readRDS('data/processed/box_surveys.rds')

# compute summary statistics by platform, n_whales and box_type
out = df %>%
  group_by(platform, n_whales, box_type) %>%
  summarize(
    platform = unique(platform),
    n_whales = unique(n_whales),
    behavior = unique(behavior),
    transits = length(unique(run)),
    mean_transit_time = mean(transit_time),
    mean_transit_dist = mean(transit_dist),
    mean_time_first_det = calc_first(x = time_first_det, y = transit_time),
    mean_dist_first_det = calc_first(x = dist_first_det, y = transit_dist),
    transits_with_detections = sum(detected),
    transit_p = transits_with_detections/transits,
    .groups = 'drop'
  )

# calculate probability by transits
probs = out %>%
  group_by(platform, n_whales, box_type) %>%
  summarize(
    n = seq(from = 1, to = 25, by = 1),
    p = 1-(1-transit_p)^n,
    .groups = 'drop'
  )

# plot --------------------------------------------------------------------

# plot p vs n_whales
ggplot()+
  geom_path(data=out,aes(x=n_whales,y=transit_p,color=platform,group=platform))+
  facet_wrap(~box_type)+
  scale_color_manual(values = platform_cols)+
  labs(x = 'Number of whales', 
       y = 'Probability of detection per transit', 
       color = 'Platform')+
  theme_bw()

# plot p vs n_surveys
prb = probs %>% filter(n_whales %in% c(1,3,5,10,25)) # choose subset to plot
ggplot()+
  geom_path(data=prb,aes(x=n,y=p,color=platform,group=platform))+
  facet_grid(box_type~n_whales)+
  scale_color_manual(values = platform_cols)+
  labs(x = 'Number of transits', 
       y = 'Probability of detection', 
       color = 'Platform')+
  theme_bw()

# plot time to first detection
ggplot()+
  geom_path(data=out,aes(x=n_whales,y=mean_time_first_det/60,color=platform,group=platform))+
  facet_wrap(~box_type)+
  scale_y_log10()+
  scale_color_manual(values = platform_cols)+
  labs(x = 'Number of whales', 
       y = 'Time to first detection (min)', 
       color = 'Platform')+
  theme_bw()

# plot distance to first detection
ggplot()+
  geom_path(data=out,aes(x=n_whales,y=mean_dist_first_det,color=platform,group=platform))+
  facet_wrap(~box_type)+
  scale_y_log10(breaks = c(0.1,1,10,100,1000,10000), labels = c(0.1,1,10,100,1000,10000))+
  scale_color_manual(values = platform_cols)+
  labs(x = 'Number of whales', 
       y = 'Distance to first detection (km)', 
       color = 'Platform')+
  theme_bw()
