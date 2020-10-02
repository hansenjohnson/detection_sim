## run_diagnostics ##
# functions for analyzing and plotting rw_sim output

# setup -------------------------------------------------------------------

library(tidyverse)

# whale_movement = readRDS('data/processed/whale_movement.rda')
whales_movement = readRDS('data/processed/multiple_whales_movement.rds')
platforms_track = readRDS('data/processed/platforms_movement.rds')
all_detections = readRDS('data/processed/all_detections.rds')

# basic detections summary -------------------------------------------------

# # basic call summary from single whale simulation
# # calculate the number of calls
# n_calls = whale_movement %>% 
#   filter(call==1) %>%
#   nrow()
# # calculate the observed call rate
# cr_obs = n_calls/((max(whale_movement$time))/60/60)
# # print message
# message('Total calls: ', n_calls)  
# message('Observed call rate: ', round(x = cr_obs, digits = 2), ' calls/whale/hr')
# # basic movement summary for single whale
# # calculate distances between each point along path
# whale_movement$dist = 0
# for(ii in 2:nrow(whale_movement)){
#   whale_movement$dist[ii] = sqrt((whale_movement$x[ii]-whale_movement$x[ii-1])^2 + 
#                                    (whale_movement$y[ii]-whale_movement$y[ii-1])^2)
#   }
# # convert to cumulative distance traveled (meters)
# whale_movement$cdist = cumsum(whale_movement$dist)
# # print message
# message('Total distance travelled by whale: ', round(max(whale_movement$cdist)), 2, 'km')

# basic call summary from multiple whales simulation
# calculate the number of calls
n_calls = whales_movement %>% 
  filter(call==1) %>%
  nrow()
# calculate the observed calls
calls_obs = all_detections %>% filter(detected==1, platform=='glider'||'buoy') %>% nrow()
# print messages
message('Total calls: ', n_calls)  
message('Calls observed: ', round(calls_obs))
message('Percent calls detected: ', round((calls_obs/n_calls) *100, digits = 2), '%')

# basic surfacing summary from multiple whales simulation
# calculate the number of surfacings

# calculate the observed surfacings

# print messages
message('Total calls: ', n_surfacings)  
message('Calls observed: ', round(surfacings_obs))
message('Percent surfacings detected: ', round((surfacings_obs/n_surfacings) *100, digits = 2), '%')

# daily presence -----------------------------------------------------------
daily_presence = all_detections %>% group_by(day,platform,detected) %>% count()

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
dt_planes = all_detections %>% filter(detected==1, platform=='plane') %>% 
  count()/max(planes_trk$time/60/60)
message('Detections per unit effort: ', round(dt_planes, digits = 3), ' detections/hour')

# vessel
vessel_trk = platforms_track %>% filter(platform=='vessel')
dt_vessel = all_detections %>% filter(detected==1, platform=='vessel') %>% 
  count()/max(vessel_trk$time/60/60)
message('Detections per unit effort: ', round(dt_vessel, digits = 3), ' detections/hour')

# detections per km surveyed  ---------------------------------------------
# find distance traveled and cumulative distance traveled
calculate_distance = function(x,y,sum_dist=TRUE){
  # count rows in df
  n = length(x)
  # define x/y vectors
  x0 = x[1:(n-1)]
  x1 = x[2:n]
  y0 = y[1:(n-1)]
  y1 = y[2:n]
  # compute distance between subsequent points and
  # pad with leading zero to match length n
  dist = c(0, sqrt((x1-x0)^2+(y1-y0)^2))
  # optionally convert to cumulative along-path distance
  if(sum_dist){
    dist = cumsum(dist)
  }
  # return distance vector
  return(dist)
}

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

# glider
# divide detections per max cumulative distance
calls_glider = all_detections %>% filter(detected==1, platform=='glider') %>% count()
dst_glider = dst %>% filter(platform=="glider")
message('Detections per unit effort: ', round(calls_glider/(dst_glider$total_distance), 
                                              digits = 3), ' detections/km')

# all planes
# divide detections per max cumulative distance
surfacings_planes = all_detections %>% filter(detected==1, platform=='plane') %>% count()
dst_planes = dst %>% filter(platform=="plane", id=='1')
message('Detections per unit effort: ', round(surfacings_planes/(dst_planes$total_distance), 
                                              digits = 3), ' detections/km')

# all vessels
# divide detections per max cumulative distance
surfacings_vessels = all_detections %>% filter(detected==1, platform=='vessel') %>% count()
dst_vessels = dst %>% filter(platform=="vessel", id=='1') 
message('Detections per unit effort: ', round(surfacings_vessels/(dst_vessels$total_distance), 
                                              digits = 3), ' detections/km')

