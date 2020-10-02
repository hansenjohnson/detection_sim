## track_distance ##
# example for calculating along-track distance

# required library
library(tidyverse)

# read in trackline data
trk = readRDS('data/processed/platforms_movement.rds')

# define distance calculation function
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
trk = trk %>%
  group_by(platform, id) %>%
  mutate(dist = calculate_distance(x=x,y=y))

# check with a quick plot
ggplot(trk, aes(x=time/60/60,y=dist,group=id))+
  geom_path()+
  facet_wrap(~platform)+
  labs(x = 'Time (hr)', y = 'Distance travelled (km)')+
  theme_bw()

# calculate the total distance covered by each survey
dst = trk %>%
  group_by(platform, id) %>%
  summarize(total_distance = max(dist, na.rm = TRUE))

# calculate detections per km ---------------------------------------------

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


