## run_diagnostics ##
# analyzing and evaluating performance of rw_sim

# setup -------------------------------------------------------------------

library(tidyverse)

# whale_movement = readRDS('data/processed/whale_movement.rda')
whales_movement = readRDS('data/processed/multiple_whales_movement.rds')
platforms_track = readRDS('data/processed/platforms_movement.rds')
all_detections = readRDS('data/processed/all_detections.rds')
planes_detections = readRDS('data/processed/planes_detections.rds')
vessels_detections = readRDS('data/processed/vessels_detections.rds')

# check observed simulation parameters -------------------------------------

# basic call summary 
# calculate the number of calls
n_calls = whales_movement %>%
  filter(call==1) %>%
  nrow()
# calculate the observed call rate
cr_obs = n_calls/((max(whales_movement$time))/60/60)/25
# print message
message('Observed call rate: ', round(x = cr_obs, digits = 2), ' calls/whale/hr')

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

# basic surfacing summary 
# find mean dive time
tmp = whales_movement %>% group_by(surface) %>% summarise(m = mean(dive_dur))
# dtime_obs = max(whales_movement$dive_time)/max(whales_movement$dive_index)
message('Observed mean dive time: ', round(x = tmp$m[tmp$surface==0], digits = 2), ' seconds')
message('Observed mean surface time: ', round(x = tmp$m[tmp$surface==1], digits = 2), ' seconds')

# check detection probability curve --------------------------------------

# for glider and buoy
# put in table
acoustic_dets = all_detections %>% filter(detected=="1",!platform=="plane", !platform=="vessel")
# plot
ggplot()+
  geom_point(data=acoustic_dets,aes(x=r_wh,y=p))

# for plane and vessel
# put in table
visual_dets = all_detections %>% filter(detected=="1", !platform=="glider", !platform=="buoy")
# plot
ggplot()+
  geom_point(data=visual_dets,aes(x=r_wh,y=p))

# plot both
ggplot()+
  geom_point(data=acoustic_dets,aes(x=r_wh,y=p), color="blue", size=0.8)+
  geom_point(data=visual_dets,aes(x=r_wh,y=p), color="purple", size=0.8)+
  theme_bw()+
  labs(x="Distance from platform (km)", y="Detection probability")
