## run_diagnostics ##
# functions for analyzing and plotting rw_sim output

# setup -------------------------------------------------------------------

library(tidyverse)

# whale_movement = readRDS('data/processed/whale_movement.rda')
whales_movement = readRDS('data/processed/multiple_whales_movement.rds')
platforms_track = readRDS('data/processed/platforms_movement.rds')
all_detections = readRDS('data/processed/all_detections.rds')
planes_detections = readRDS('data/processed/planes_detections.rds')
vessels_detections = readRDS('data/processed/vessels_detections.rds')

# basic detections summary -------------------------------------------------

# # # basic call summary from single whale simulation
# # # calculate the number of calls
# # n_calls = whale_movement %>% 
# #   filter(call==1) %>%
# #   nrow()
# # # calculate the observed call rate
# # cr_obs = n_calls/((max(whale_movement$time))/60/60)
# # # print message
# # message('Total calls: ', n_calls)  
# # message('Observed call rate: ', round(x = cr_obs, digits = 2), ' calls/whale/hr')
# # # basic movement summary for single whale
# # # calculate distances between each point along path
# # whale_movement$dist = 0
# # for(ii in 2:nrow(whale_movement)){
# #   whale_movement$dist[ii] = sqrt((whale_movement$x[ii]-whale_movement$x[ii-1])^2 + 
# #                                    (whale_movement$y[ii]-whale_movement$y[ii-1])^2)
# #   }
# # # convert to cumulative distance traveled (meters)
# # whale_movement$cdist = cumsum(whale_movement$dist)
# # # print message
# # message('Total distance travelled by whale: ', round(max(whale_movement$cdist)), 2, 'km')
# 
# # basic call summary from multiple whales simulation
# # calculate the number of calls
# n_calls = whales_movement %>% 
#   filter(call==1) %>%
#   nrow()
# # calculate the observed calls
# calls_obs = all_detections %>% filter(detected==1, platform=='glider'||'buoy') %>% nrow()
# # print messages
# message('Total calls: ', n_calls)  
# message('Calls observed: ', round(calls_obs))
# message('Percent calls detected: ', round((calls_obs/n_calls) *100, digits = 2), '%')
# 
# # basic surfacing summary from multiple whales simulation
# # calculate the number of surfacings
# 
# # calculate the observed surfacings
# 
# # print messages
# message('Total calls: ', n_surfacings)  
# message('Calls observed: ', round(surfacings_obs))
# message('Percent surfacings detected: ', round((surfacings_obs/n_surfacings) *100, digits = 2), '%')

# correct surfacing detections ---------------------------------------------

# for planes
# count number of detections
planes_det_orig = planes_detections %>% filter(detected==1) %>% nrow()
txt1 = paste0('Whale detected on ', round(planes_det_orig/nrow(planes_detections)*100,2), 
              '% of timesteps (', planes_det_orig, '/', nrow(planes_detections), ')')
# count detections per surfacing
planes_srf_df = planes_detections %>%
  mutate(detected=as.numeric(detected)) %>%
  group_by(id,dive_index) %>%
  summarize(
    x_wh = mean(x_wh),
    y_wh = mean(y_wh),
    time = mean(time),
    r_wh = mean(r_wh),
    p = mean(p),
    detected = sum(detected)
  )
# convert to binary (0,1) detection
planes_srf_df$detected[planes_srf_df$detected>0]=1
# count number of surface detections
planes_det_surf = planes_srf_df %>% filter(detected==1) %>% nrow()
txt2 = paste0('Whale detected on ', round(planes_det_surf/nrow(planes_srf_df)*100,2), 
              '% of surfacings (', planes_det_surf, '/', nrow(planes_srf_df), ')')

# for vessels
# count number of detections
vessels_det_orig = vessels_detections %>% filter(detected==1) %>% nrow()
txt3 = paste0('Whale detected on ', round(vessels_det_orig/nrow(vessels_detections)*100,2), 
              '% of timesteps (', vessels_det_orig, '/', nrow(vessels_detections), ')')
# count detections per surfacing
vessels_srf_df = vessels_detections %>%
  mutate(detected=as.numeric(detected)) %>%
  group_by(id,dive_index) %>%
  summarize(
    x_wh = mean(x_wh),
    y_wh = mean(y_wh),
    time = mean(time),
    r_wh = mean(r_wh),
    p = mean(p),
    detected = sum(detected)
  )
# convert to binary (0,1) detection
vessels_srf_df$detected[vessels_srf_df$detected>0]=1
# count number of surface detections
vessels_det_surf = vessels_srf_df %>% filter(detected==1) %>% nrow()
txt4 = paste0('Whale detected on ', round(vessels_det_surf/nrow(vessels_srf_df)*100,2), 
              '% of surfacings (', vessels_det_surf, '/', nrow(vessels_srf_df), ')')

# daily presence -----------------------------------------------------------

# correct to include surfacing detections not total time step detections
all_detections = all_detections %>% filter(!platform=="plane") %>% filter(!platform=="vessel") %>%
  mutate(detected=as.numeric(detected))

planes_srf_df = planes_srf_df %>%
  select(-dive_index) %>%
  mutate(platform = 'plane')

vessels_srf_df = vessels_srf_df %>%
  select(-dive_index) %>%
  mutate(platform = 'vessel')

tbins = seq(from = 0, to = 24*7, by = 24)
planes_srf_df$day = cut(planes_srf_df$time/60/60, breaks = tbins, include.lowest = T)
vessels_srf_df$day = cut(vessels_srf_df$time/60/60, breaks = tbins, include.lowest = T)

all_det_corrected = bind_rows(all_detections,planes_srf_df,vessels_srf_df)
daily_presence = all_det_corrected %>% group_by(day,platform,detected) %>% count()

# count days with detections for each platform
#glider
glider_daily_det = daily_presence %>% filter(platform=='glider',detected==1) %>% nrow()
txt5 = paste0('Whale detected by glider on ', glider_daily_det, 
              '/7 days')

# mooring
buoy_daily_det = daily_presence %>% filter(platform=='buoy',detected==1) %>% nrow()
txt6 = paste0('Whale detected by buoy on ', buoy_daily_det, 
              '/7 days')

# plane
plane_daily_det = daily_presence %>% filter(platform=='plane',detected==1) %>% nrow()
txt7 = paste0('Whale detected by plane on ', plane_daily_det, 
              '/7 days')

# vessel
vessel_daily_det = daily_presence %>% filter(platform=='vessel',detected==1) %>% nrow()
txt8 = paste0('Whale detected by vessel on ', vessel_daily_det, 
              '/7 days')

# detections per time surveyed  --------------------------------------------
# glider
glider_trk = platforms_track %>% filter(platform=='glider')
dt_glider = all_detections %>% filter(detected==1, platform=='glider') %>% 
  count()/max(glider_trk$time/60/60)
message('Detections per unit effort: ', round(dt_glider, digits = 3), ' detections/hour')

# mooring
buoy_trk = platforms_track %>% filter(platform=='buoy')
dt_buoy = all_detections %>% filter(detected==1, platform=='buoy') %>% 
  count()/max(buoy_trk$time/60/60)
message('Detections per unit effort: ', round(dt_buoy, digits = 3), ' detections/hour')

# planes
planes_trk = platforms_track %>% filter(platform=='plane')
dt_planes = planes_det_surf/max(planes_trk$time/60/60)
message('Detections per unit effort: ', round(dt_planes, digits = 3), ' detections/hour')

# vessel
vessel_trk = platforms_track %>% filter(platform=='vessel')
dt_vessel = vessels_det_surf/max(vessel_trk$time/60/60)
message('Detections per unit effort: ', round(dt_vessel, digits = 3), ' detections/hour')

# detections per km surveyed  ---------------------------------------------
# calculate along-track distance by platform and survey id
platforms_track = platforms_track %>%
  group_by(platform, id) %>%
  mutate(dist = calculate_distance(x=x,y=y))

# check with a quick plot
ggplot(platforms_track, aes(x=time/60/60,y=dist,group=id))+
  geom_path()+
  facet_wrap(~platform)+
  labs(x = 'Time (hr)', y = 'Distance travelled (km)')+
  theme_bw()

# calculate the total distance covered by each survey
dst = platforms_track %>%
  group_by(platform, id) %>%
  summarize(total_distance = max(dist, na.rm = TRUE))

# calculate total trackline effort by each platform
total_dst = dst %>%
  group_by(platform) %>%
  summarize(total_distance = sum(total_distance))

# calculate total detected per platform
total_det = readRDS('data/processed/all_detections.rds') %>%
  mutate(detected=as.numeric(detected))%>%
  filter(detected == 1) %>%
  group_by(platform) %>%
  summarise(total_detections = sum(detected))

# combine and compute detections per km
total = left_join(total_dst,total_det) %>%
  mutate(detections_per_km = total_detections/total_distance)

# glider
message('Detections per unit effort: ', 
        round(total[2,4], digits = 3), ' detections/km')
# planes
message('Detections per unit effort: ', 
        round(planes_det_surf/total_dst[3,2], digits = 3), ' detections/km')
# vessels
message('Detections per unit effort: ', 
        round(vessels_det_surf/total_dst[4,2], digits = 3), ' detections/km')

