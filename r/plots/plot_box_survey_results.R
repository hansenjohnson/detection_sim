## plot_box_survey_results ##
# summarize and plot output from box survey simulation

# setup -------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
source('r/box_survey_functions.R')

# define colors for each platform
platform_cols = c('Slocum glider' = 'blue', 'Aircraft' = 'red', 
                  'Vessel' = 'black', 'RPAS' = 'lightslategrey')

# process -----------------------------------------------------------------

# read in simulated survey data
# note that some lines for the TC box are missing (kaos crashed)
df = readRDS('data/processed/box_surveys.rds')

# change platform names for plotting
df$platform = recode(df$platform, slocum = "Slocum glider", plane = "Aircraft", 
                     vessel = "Vessel", rpas = "RPAS")

# define platform factor for plotting order
df$platform = factor(df$platform, levels = c("Aircraft", "RPAS", "Vessel", "Slocum glider"), ordered = TRUE)

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
       y = expression(P[T]*'(1)'), 
       color = 'Platform',
       linetype = 'Domain')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"), 
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(), 
        panel.border = element_blank())
p

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
       y = expression(P[T]*'(n)'), 
       color = 'Platform',
       linetype = 'Domain')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"), 
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank())
  )
q

# save plot
ggsave('figures/per_transits_box_surveys.png', q, height = 5, width = 5, units = 'in', dpi = 300)

# combine both plots into one and save
r = ggarrange(p,q, ncol=2, nrow=1, common.legend=TRUE, labels = c('a)','b)'), legend = 'right')
r
ggsave('figures/figure_4.pdf', r, height = 5, width = 10, units = 'in', dpi = 300)

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

# det prob examples ------------------------------------------------------------

## calculations ##

# calculate number of transits needed for 50% det prob

probs = probs %>%
  dplyr::filter(p >= 0.5)

det_prob_example = probs %>%
  group_by(platform, n_whales, box_type) %>%
  summarize(
    transits_50_prob = n[which.min(abs(p-0.5))], 
    # finds the number of transits needed for each combination of 
    # platform/whale number/zone to approach a 0.5 probability as close as possible
    .groups = 'drop'
  )

# repeat for 95% det prob

probs = probs %>%
  dplyr::filter(p >= 0.95)

det_prob_example2 = probs %>%
  group_by(platform, n_whales, box_type) %>%
  summarize(
    transits_95_prob = n[which.min(abs(p-0.95))], 
    .groups = 'drop'
  )

# put in a single table
det_prob = full_join(det_prob_example, det_prob_example2)

# format transit times (in hours)
tt = transit_times %>% transmute(platform, box_type, transit_time_h = hours)

# define hourly costs
tc = tibble(platform = c('Aircraft', 'RPAS', 'Vessel', 'Slocum glider'), cost_h = c(1592,NA,700,31.25))

# add transit times to probs
d1 = left_join(det_prob, tt)

# add hourly cost to probs
d2 = left_join(d1, tc)

# calc time to example probs
d2$time_50_prob = d2$transits_50_prob*d2$transit_time_h
d2$time_95_prob = d2$transits_95_prob*d2$transit_time_h

# calc dist to 50% prob
# d2$dist_50_prob = d2$transits_50_prob*out$mean_transit_dist

# calc area to 50% prob
# d2$area_50_prob = d2$transits_50_prob*out$mean_transit_area

# calc cost to example probs
d2$cost_50 = d2$time_50_prob*d2$cost_h
d2$cost_95 = d2$time_95_prob*d2$cost_h

## test plots ##

# Number of transits to example detections
ggplot(d2)+
  geom_path(aes(x=n_whales,y=transits_50_prob,group=platform,color=platform))+
  scale_color_manual(values = platform_cols)+
  facet_wrap(~box_type, nrow = 1)+
  labs(x = 'Number of whales', y = 'Number of transits to 50% detection', color = 'Platform')+
  theme_bw()

ggplot(d2)+
  geom_path(aes(x=n_whales,y=transits_95_prob,group=platform,color=platform))+
  scale_color_manual(values = platform_cols)+
  facet_wrap(~box_type, nrow = 1)+
  labs(x = 'Number of whales', y = 'Number of transits to 95% detection', color = 'Platform')+
  theme_bw()

# Transit time to 50% detection
ggplot(d2)+
  geom_path(aes(x=n_whales,y=time_50_prob,group=platform,color=platform))+
  scale_color_manual(values = platform_cols)+
  facet_wrap(~box_type, nrow = 1)+
  labs(x = 'Number of whales', y = 'Transit time (h) to 50% detection', color = 'Platform')+
  theme_bw()

# Transit dist to 50% detection
# ggplot(d2)+
#   geom_path(aes(x=n_whales,y=dist_50_prob,group=platform,color=platform))+
#   scale_color_manual(values = platform_cols)+
#   facet_wrap(~box_type, nrow = 1)+
#   labs(x = 'Number of whales', y = 'Transit dist (km) to 50% detection', color = 'Platform')+
#   theme_bw()

# Transit area to 50% detection
# ggplot(d2)+
#   geom_path(aes(x=n_whales,y=area_50_prob,group=platform,color=platform))+
#   scale_color_manual(values = platform_cols)+
#   facet_wrap(~box_type, nrow = 1)+
#   labs(x = 'Number of whales', y = 'Transit area (km^2) to 50% detection', color = 'Platform')+
#   theme_bw()

# Cost to 50% detection
ggplot(d2)+
  geom_path(aes(x=n_whales,y=cost_50,group=platform,color=platform))+
  scale_color_manual(values = platform_cols)+
  facet_wrap(~box_type, nrow = 1)+
  labs(x = 'Number of whales', y = 'Cost to 50% detection', color = 'Platform')+
  theme_bw()

# plot all together

# remove time per transit and cost per hour columns
d2 = subset(d2, select = -c(transit_time_h,cost_h))

# pivot df and limit whale number for plotting
d3 = d2 %>%
  pivot_longer(transits_50_prob:cost_95, names_to = 'vars', values_to = 'vals') %>%
  dplyr::select(platform, n_whales, box_type, vars, vals) %>%
  dplyr::filter(n_whales <= 15)

# define factors for plotting order
d3$vars = factor(d3$vars, levels = c("transits_50_prob", "transits_95_prob", "time_50_prob", 
                                     "time_95_prob", "cost_50", "cost_95"), ordered = TRUE)

# define factors for plotting order
d3$platform = factor(d3$platform, levels = c("Aircraft","RPAS","Vessel","Slocum glider"), ordered = TRUE)

# prepare metric labels for plotting
d3$var_labels = factor(d3$vars, labels = c(`transits_50_prob`="N[0.5]~(transits)", 
                                           `transits_95_prob`="N[0.95]~(transits)",
                                           `time_50_prob`="T[0.5]~(hours)",
                                           `time_95_prob`="T[0.95]~(hours)",
                                           `cost_50`='C[0.5]~("$")',
                                          `cost_95`=('C[0.95]~("$")')),ordered = TRUE)

# subset for plotting
d3_dfo = d3 %>% dplyr::filter(box_type == 'DFO')
d3_tc = d3 %>% dplyr::filter(box_type == 'TC')

# construct labels for each subplot
plot_labs = d3_dfo %>% 
  dplyr::filter(!is.infinite(vals)) %>% # remove infinite cost
  group_by(var_labels) %>% 
  summarize(vals = 0.9*max(vals, na.rm = TRUE)) %>%
  mutate(label = c('a)', 'b)', 'c)', 'd)', 'e)', 'f)'),
         n_whales = 15) # set to 0 for left-justified letters

# plot
s = ggplot()+
  geom_path(data = d3_dfo, aes(x = n_whales, y = vals, group = platform, color = platform))+
  #geom_path(data = d3_tc, aes(x = n_whales, y = vals, group = platform, color = platform))+
  geom_text(data = plot_labs, aes(x = n_whales, y = vals, label = label)) +
  scale_color_manual(values = platform_cols)+
  labs(x='Number of whales', y='Performance metric value', color = ' Platforms')+
  facet_wrap(~var_labels, scales='free_y', ncol = 2, strip.position = 'right', labeller=label_parsed)+
  theme_bw()
s

# make label text bigger
s = s + theme(text = element_text(size = 20))

ggsave('figures/figure_5.pdf', s, height = 10, width = 15, units = 'in', dpi = 300)
