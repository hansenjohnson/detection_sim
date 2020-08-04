## make_track ##
# simulate a trackline from list of waypoints

# input -------------------------------------------------------------------

# waypoint file
ifile = 'data/raw/waypoints.csv'

# platform speed (m/s)
spd = 0.1

# track time resolution (s)
res = 60

# track file
ofile = 'data/processed/track.rds'

# setup -------------------------------------------------------------------

library(tidyverse)
library(oce)
library(zoo)

# process -----------------------------------------------------------------

# read in waypoint list
wpts = read_csv(ifile, col_types = cols())

# calculate distances between each point along path (meters)
wpts$dist = 0
for(ii in 2:nrow(wpts)){
  wpts$dist[ii] = sqrt((wpts$x[ii]-wpts$x[ii-1])^2 + (wpts$y[ii]-wpts$y[ii-1])^2)
}

# convert to cumulative distance traveled (meters)
wpts$cdist = cumsum(wpts$dist)

# calculate time to distance (seconds)
wpts$time = wpts$cdist/spd

# create time sequence (seconds)
tseq = seq(from = res, to = ceiling(max(wpts$time)), by = res)

# create an empty trackline grid
tmp = tibble(x=NA, y=NA, time = tseq)

# join, remove extra columns, and arrange in time
trk = full_join(tmp,wpts,by=c("x","y","time")) %>%
  transmute(x, y, time) %>%
  arrange(time)

# interpolate
trk$x = na.approx(trk$x)
trk$y = na.approx(trk$y)

# remove waypoint times (keeps sampling even)
trk = trk[trk$time %in% c(0,tseq),]

# plot to check
ggplot(data = trk, aes(x=x,y=y))+
  geom_path()+
  geom_point(shape=1)+
  coord_equal()+
  theme_bw()

# print diagnostics
message('Total number of waypoints: ', nrow(wpts))
message('Total path distance: ', max(wpts$dist)/1e3, ' km')
message('Total transit time: ', round(max(wpts$time)/60/60, 2), ' hr')

# save
saveRDS(object = trk, file = ofile)
message('Track saved as: ', ofile)
