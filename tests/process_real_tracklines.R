## real_glider_tracks ##
# processing real glider tracks to use in simulation

# input -------------------------------------------------------------------

# waypoint file
ifile = 'data/raw/PilotStudy_FinalWaypoints.csv'

# setup -------------------------------------------------------------------

library(oce)
source('r/rw_sim.R')

# process ------------------------------------------------------------------

# read in waypoint list
wpts = read_csv(ifile, col_types = cols())

# remove NAs
wpts = wpts[complete.cases(wpts),]

# identify lon and lat from file
lon=wpts$X
lat=wpts$Y

# transform to local coordinates (in meters)
wpts = geodXy(lon, lat, lon[1], lat[1])

# ad waypoint id column
wpts$id = seq(from=1, to = nrow(wpts))

# save
write.csv(wpts, 'data/raw/waypoints_real_GSL_glider.csv')
message('Processed glider data saved as: ', 'data/raw/waypoints_real_GSL_glider.csv')

# test --------------------------------------------------------------------

res = 60

track_df = make_track(waypoints = 'data/raw/waypoints_real_GSL_glider.csv', res = res, spd = 0.1)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )

# plot
ggplot()+
  geom_path(data = track_df, aes(x=x,y=y), color = 'blue')+
  #geom_path(data = wh, aes(x=x,y=y), color = 'black')+
  #geom_point(shape=1)+
  coord_equal()+
  theme_bw()
