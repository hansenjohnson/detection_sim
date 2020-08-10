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

source('r/rw_sim.R')
       
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

# simulate single whale moving
# model parameters
hrs = 24*7 # model run length (hr)
x0 = 3000 # start x coord (m)
y0 = 1000 # start y coord (m)

# run model
wh = rw_sim(hrs = hrs, bh = 'feeding', sub = TRUE, nt = res, x0=x0, y0=y0)

# plot to check
ggplot()+
  geom_path(data = wh,aes(x=x,y=y))+
  geom_point(data = filter(wh,call==1),aes(x=x,y=y),shape=21,fill='red')+
  coord_fixed()

# create time sequence (seconds)
tseq = seq(from = res, to = ceiling(max(wh$t)), by = res)

# create an empty trackline grid
tmp = tibble(x=NA, y=NA, time = tseq)

# join, remove extra columns, and arrange in time
trk = full_join(tmp,wpts,by=c("x","y","time")) %>%
  transmute(x, y, time) %>%
  arrange(time)

# remove times after last waypoint is reached
trk = trk %>% filter(time<=max(wpts$time))

# interpolate (finds x and y positions for the times in between the waypoints)
trk$x = na.approx(trk$x)
trk$y = na.approx(trk$y)

# remove waypoint times (keeps sampling even)
trk = trk[trk$time %in% c(0,tseq),]

# plot to check
ggplot()+
  geom_path(data = trk, aes(x=x,y=y), color = 'blue')+
  geom_path(data = wh, aes(x=x,y=y), color = 'black')+
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

# test function ------------------------------------------------------------

# generate track
trk = make_track(waypoints = 'data/raw/waypoints.csv')

# plot to check
ggplot()+
  geom_path(data = trk, aes(x=x,y=y), color = 'blue')+
  geom_path(data = wh, aes(x=x,y=y), color = 'black')+
  geom_point(shape=1)+
  coord_equal()+
  theme_bw()

