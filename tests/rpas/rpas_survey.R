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
  df = run_box_surveys(height = 18, width = 12, n_surveys = 10, n_whales = c(1,5,10))
  
  # save
  saveRDS(df, 'tests/rpas/surveys.rds')
} else {
  df = readRDS(ofile)
}

# process -----------------------------------------------------------------

# count detections per surfacing
df_surf = df %>%
  group_by(whale_id,dive_index) %>%
  summarize(
    run = unique(run),
    platform = unique(platform),
    whale_id = unique(whale_id),
    n_whales = unique(n_whales),
    behavior = unique(behavior),
    dive_index = unique(dive_index),
    transits = length(unique(run)),
    mean_transit_time = mean(transit_time, na.rm = TRUE),
    mean_transit_dist = mean(transit_dist, na.rm = TRUE),
    n_detected,
    detected = sum(detected)
  )

# convert to binary (0,1) detection
df_surf$detected[df_surf$detected>0]=1

# summarize
out = df_surf %>%
  group_by(platform,n_whales) %>%
  summarize(
    platform = unique(platform),
    n_whales = unique(n_whales),
    behavior = unique(behavior),
    transits,
    transits_with_detections = sum(detected),
    transit_p = transits_with_detections/transits,
    mean_transit_time,
    mean_transit_dist,
    mean_detections = mean(sum(detected), na.rm = TRUE),
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
    det_per_time = mean_detections/mean_transit_time*60*60, # per hour
    det_per_dist = mean_detections/mean_transit_dist,
    cost_per_hour = NA,
    .groups = 'drop'
  )

# add the cost per hour for every platform
metrics_plane = metrics %>% filter(platform=='plane')
metrics_plane$cost_per_hour = 1592
metrics_rpas = metrics %>% filter(platform=='rpas')
metrics_rpas$cost_per_hour = NA
metrics_slocum = metrics %>% filter(platform=='slocum')
metrics_slocum$cost_per_hour = 31.25
metrics_vessel = metrics %>% filter(platform=='vessel')
metrics_vessel$cost_per_hour = 1350

# find the cost/det for each platform and whale combination
metrics = bind_rows(metrics_plane,metrics_rpas, metrics_slocum, metrics_vessel)
metrics$cost_per_det = metrics$cost_per_hour/metrics$det_per_time

# delete cost per hour column
metrics = metrics %>% subset(select = -cost_per_hour)
