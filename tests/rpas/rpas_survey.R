## rpas_survey ##
# test to add rpas surveys


# input -------------------------------------------------------------------

ofile = 'tests/rpas/surveys.rds'

# setup -------------------------------------------------------------------

set.seed(123)
source('tests/rpas/box_survey_functions.R')

# process -----------------------------------------------------------------

if(!file.exists(ofile)){
  
  # simulate rpas surveys and return data
  df_rpas = box_surveys(
    platform = 'rpas',
    height = 18,
    width = 12,
    nrws = 60,
    n_surveys = 3,
    bh = 'feeding',
    whales_parallel = TRUE,
    survey_parallel = FALSE,
    include_data = TRUE
  )
  
  # show plot
  df_rpas$plot[[1]]
  
  # simulate plane surveys and return data
  df_plane = box_surveys(
    platform = 'plane',
    height = 18,
    width = 12,
    nrws = 60,
    n_surveys = 3,
    bh = 'feeding',
    whales_parallel = TRUE,
    survey_parallel = FALSE,
    include_data = TRUE
  )
  
  # show plot
  df_plane$plot[[2]]
  
  # run surveys
  df = run_box_surveys(height = 18, width = 12, n_surveys = 10, n_whales = c(1,5,10,15,30,60), 
                       whales_parallel = FALSE, survey_parallel = FALSE)
  
  # save
  saveRDS(df, file = ofile)
} else {
  df = readRDS(ofile)
}

# process -----------------------------------------------------------------

# summarize
out = df %>%
  group_by(platform,n_whales) %>%
  summarize(
    platform = unique(platform),
    n_whales = unique(n_whales),
    behavior = unique(behavior),
    transits = length(unique(run)),
    transits_with_detections = sum(detected),
    transit_p = transits_with_detections/transits,
    mean_transit_time = mean(transit_time, na.rm = TRUE),
    mean_transit_dist = mean(transit_dist, na.rm = TRUE),
    mean_detections = mean(n_detected, na.rm = TRUE),
    det_per_time = mean_detections/mean_transit_time*60*60, # per hour
    det_per_dist = mean_detections/mean_transit_dist,
    .groups = 'drop'
  )

# check 
out

# plot
p = ggplot()+
  geom_path(data=out,aes(x=n_whales,y=transit_p,color=platform,group=platform))+
  scale_color_manual(values = c('slocum' = 'blue', 'plane' = 'red', 'vessel' = 'darkslategrey', 'wave' = 'cornsilk3', 'rpas' = 'purple'))+
  labs(x = 'Number of whales', 
       y = 'Probability of detection per transit', 
       color = 'Platform')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())
p

q = ggplot() +
  geom_point(data=out,aes(x=det_per_time,y=det_per_dist,shape=platform), size = 3)+
  scale_shape_manual(values = c(1,2,3,4,5))+
  labs(x = 'Detections per time (det/hr)', 
       y = 'Detections per distance (det/km)', 
       color = 'Platform')+
  facet_wrap(~n_whales)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

q

#find the det/time and det/dist for each platform and whale combination
metrics = out %>%
  group_by(platform, n_whales) %>%
  summarize(
    platform = unique(platform),
    n_whales = unique(n_whales),
    box_type = 'DFO',
    mean_detections,
    transit_p,
    det_per_hour = mean_detections/mean_transit_time*60*60, # per hour
    det_per_dist = mean_detections/mean_transit_dist,
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
metrics = metrics %>% select(-cost_per_hour)
