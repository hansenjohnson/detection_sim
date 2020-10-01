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

# detections per unit effort  ----------------------------------------------
# per time surveyed
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

# per km surveyed (for moving platforms)
# glider
# find distance traveled and cumulative distance traveled
glider_trk$dist = 0
for(ii in 2:nrow(glider_trk)){
  glider_trk$dist[ii] = sqrt((glider_trk$x[ii]-glider_trk$x[ii-1])^2 
                             + (glider_trk$y[ii]-glider_trk$y[ii-1])^2)
}
glider_trk$cdist = cumsum(glider_trk$dist)
# divide detections per max cumulative distance
dkm_glider = all_detections %>% filter(detected==1, platform=='glider') %>% 
  count()/max(glider_trk$cdist)
message('Detections per unit effort: ', round(dkm_glider, digits = 3), ' detections/km')

# planes
# find distance traveled and cumulative distance traveled
planes_trk$dist = 0

for(ii in 2:nrow(planes_trk)){
  planes_trk$dist[ii] = sqrt((planes_trk$x[ii]-planes_trk$x[ii-1])^2 
                             + (planes_trk$y[ii]-planes_trk$y[ii-1])^2)
}

# divide detections per max cumulative distance

message('Detections per unit effort: ', round(dkm_planes, digits = 3), ' detections/km')

# vessel
# find distance traveled and cumulative distance traveled
vessel_trk$dist = 0

for(ii in 2:nrow(vessel_trk)){
  vessel_trk$dist[ii] = sqrt((vessel_trk$x[ii]-vessel_trk$x[ii-1])^2 
                             + (vessel_trk$y[ii]-vessel_trk$y[ii-1])^2)
}

# divide detections per max cumulative distance

message('Detections per unit effort: ', round(dkm_vessel, digits = 3), ' detections/km')

