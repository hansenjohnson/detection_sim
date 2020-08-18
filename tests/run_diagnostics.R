## run_diagnostics ##
# functions for analyzing and plotting rw_sim output

# setup -------------------------------------------------------------------

library(tidyverse)

# process ------------------------------------------------------------------

whale_movement = readRDS('data/processed/whale_movement.rda')
whales_movement = readRDS('data/processed/multiple_whales_movement.rda')
glider_track = readRDS('data/processed/glider_movement.rda')
call_detections = readRDS('data/processed/call_detections.rda')

# basic call summary from single whale simulation
# calculate the number of calls
n_calls = whale_movement %>% 
  filter(call==1) %>%
  nrow()
# calculate the observed call rate
cr_obs = n_calls/((max(whale_movement$time))/60/60)
# print message
message('Total calls: ', n_calls)  
message('Observed call rate: ', round(x = cr_obs, digits = 2), ' calls/whale/hr')

# basic call summary from multiple whales simulation
# calculate the number of calls
n_calls = whales_movement %>% 
  filter(call==1) %>%
  nrow()
# calculate the observed call rate
cr_obs = n_calls/((max(whales_movement$time))/60/60)/length(unique(whales_movement$id))
# print message
message('Total calls: ', n_calls)  
message('Observed call rate: ', round(cr_obs, digits = 2))

# basic movement summary for single whale
# calculate distances between each point along path
whale_movement$dist = 0
for(ii in 2:nrow(whale_movement)){
  whale_movement$dist[ii] = sqrt((whale_movement$x[ii]-whale_movement$x[ii-1])^2 + 
                                   (whale_movement$y[ii]-whale_movement$y[ii-1])^2)
  }
# convert to cumulative distance traveled (meters)
whale_movement$cdist = cumsum(whale_movement$dist)
# print message
message('Total distance travelled by whale: ', round(max(whale_movement$cdist)), 2, 'km')

# basic call detection summary 
# find total number of detected calls
detections = call_detections %>% filter(detected==1)
# print diagnostics
message('Number of calls unavailable to platform: ', n_calls-(nrow(call_detections)))
message('Number of calls available: ', nrow(call_detections))
message('Number of calls detected: ', nrow(detections))
message('Percent detection efficiency: ', 100*round((nrow(detections))/(nrow(call_detections)), 2), '%')
