## plot_box_survey_results ##
# summarize and plot output from box survey simulation

# setup -------------------------------------------------------------------

library(tidyverse)
source('r/box_survey_functions.R')

# define colors for each platform
platform_cols = c('glider' = 'blue', 'plane' = 'red', 'vessel' = 'darkslategrey')

# process -----------------------------------------------------------------

# read in simulated survey data
# note that some lines for the TC box are missing (kaos crashed)
df = readRDS('data/processed/box_surveys.rds')

# compute summary statistics by platform, n_whales and box_type
out = df %>%
  group_by(platform, n_whales, box_type) %>%
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
    .groups = 'drop'
  )

# compute summary statistics of cues per whale

# separate by box_type to be able to plot in one graph 
out_dfo = filter(out,box_type=="DFO")
out_tc = filter(out,box_type=="TC")

# calculate probability by transits for second plot
probs = out %>%
  group_by(platform, n_whales, box_type) %>%
  summarize(
    n = seq(from = 1, to = 70, by = 1),
    p = 1-(1-transit_p)^n,
    .groups = 'drop'
  )

# separate by box_type to be able to plot in one graph 
probs_dfo = filter(probs,box_type=="DFO")
probs_tc = filter(probs,box_type=="TC")

# calculate average transit times
transit_times = df %>%
  group_by(platform, box_type) %>%
  summarize(
    seconds = mean(transit_time, na.rm = T),
    minutes = seconds/60,
    hours = minutes/60,
    .groups = 'drop'
  )

# plot --------------------------------------------------------------------

# plot p vs n_whales
p = ggplot()+
  geom_path(data=out_dfo,aes(x=n_whales,y=transit_p,color=platform,group=platform,linetype=box_type))+
  geom_path(data=out_tc,aes(x=n_whales,y=transit_p,color=platform,group=platform,linetype=box_type))+
  scale_color_manual(values = platform_cols)+
  labs(x = 'Number of whales', 
       y = 'Probability of detection per transit', 
       color = 'Platform',
       linetype = 'Domain')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

# save plot
ggsave('figures/per_whales_box_surveys.png', p, height = 5, width = 5, units = 'in', dpi = 300)

# plot p vs n_surveys
# choose subset to plot
prb_dfo = probs_dfo %>% filter(n_whales %in% c(1,3,5,10)) 
prb_tc = probs_tc %>% filter(n_whales %in% c(1,3,5,10)) 

q = ggplot()+
  geom_path(data=prb_dfo,aes(x=n,y=p,color=platform,group=platform,linetype=box_type))+
  geom_path(data=prb_tc,aes(x=n,y=p,color=platform,group=platform,linetype=box_type))+
  facet_grid(~n_whales)+
  scale_color_manual(values = platform_cols)+
  labs(x = 'Number of transits', 
       y = 'Probability of detection', 
       color = 'Platform',
       linetype = 'Domain')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# save plot
ggsave('figures/per_transits_box_surveys.png', q, height = 5, width = 5, units = 'in', dpi = 300)

# plot time to first detection
# r = ggplot()+
#   geom_path(data=out,aes(x=n_whales,y=mean_time_first_det/60,color=platform,group=platform))+
#   facet_wrap(~box_type)+
#   scale_y_log10()+
#   scale_color_manual(values = platform_cols)+
#   labs(x = 'Number of whales', 
#        y = 'Time to first detection (log(min))', 
#        color = 'Platform')+
#   theme_bw()+
#   theme(axis.line = element_line(colour = "black"), 
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank())
# 
# # save plot
# ggsave('figures/t_first_det.png', r, height = 5, width = 5, units = 'in', dpi = 300)

# plot distance to first detection
# s = ggplot()+
#   geom_path(data=out,aes(x=n_whales,y=mean_dist_first_det,color=platform,group=platform))+
#   facet_wrap(~box_type)+
#   scale_y_log10(breaks = c(0.1,1,10,100,1000,10000), labels = c(0.1,1,10,100,1000,10000))+
#   scale_color_manual(values = platform_cols)+
#   labs(x = 'Number of whales', 
#        y = 'Distance to first detection (log(km))', 
#        color = 'Platform')+
#   theme_bw()+
#   theme(axis.line = element_line(colour = "black"), 
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank())

# save plot
# ggsave('figures/dist_first_det.png', s, height = 5, width = 5, units = 'in', dpi = 300)

# cite packages
citation(package='tidyverse')
citation(package='oce')
citation(package='zoo')
citation(package='ggplot2')
citation()

