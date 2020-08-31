## detection_model_example ##
# run through a fully-fledged example with all functions read in

# setup -------------------------------------------------------------------

source('r/rw_sim.R')

ofile_wh = 'data/processed/whale_movement.rda'
ofile_whs = 'data/processed/multiple_whales_movement.rda'
ofile_trk = 'data/processed/glider_movement.rda'
ofile_det = 'data/processed/call_detections.rda'

# process -----------------------------------------------------------------

# define time resolution (s)
res = 60

# produce whale movement model and convert to km
whale_df = rw_sim(hrs=24, nt = res, sub = TRUE, x0 = 5000, y0 = 5000, bh = 'feeding', cr_mn_hr=1)%>%
  mutate(
    x=x/1000,
    y=y/1000,
    r=r/1000
  )

# produce glider track and convert to km
track_df = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 51)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )

# simulate detection capabilities of glider
det_df = simulate_detections(whale_df = whale_df, track_df = track_df, det_method = 'visual')

# save
saveRDS(object = whale_df, file = ofile_wh)
message('Processed glider data saved as: ', ofile_wh)
saveRDS(object = track_df, file = ofile_trk)
message('Processed glider data saved as: ', ofile_trk)
saveRDS(object = det_df, file = ofile_det)
message('Processed glider data saved as: ', ofile_det)

# plot --------------------------------------------------------------------

# plot whale with calls and surfacing
ggplot()+
  geom_path(data = whale_df, aes(x=x,y=y,group=dive_index,color=surface))+
  scale_color_manual(values = c('0'='grey', '1'='black'))+
  geom_point(data=filter(whale_df,call==1), aes(x=x,y=y), shape = 21, fill = 'red')+
  coord_equal()+
  theme_bw()+
  labs(x="x (km)", y="y (km)", title="Simulated Whale Moving", subtitle="red dots indicate calls produced")

# plot both tracks (no detections)
ggplot()+
  geom_path(data = track_df, aes(x=x,y=y), color = 'blue')+
  geom_path(data = whale_df, aes(x=x,y=y), color = 'black')+
  geom_point(shape=1)+
  coord_equal()+
  theme_bw()

# plot both tracks with calls detected
ggplot()+
  # plot glider track
  geom_path(data = track_df,aes(x=x,y=y), color = 'black')+
  # plot whale track
  geom_path(data = whale_df,aes(x=x,y=y), color = 'grey')+
  # plot calls
  geom_point(data = det_df,aes(x=x_wh,y=y_wh,fill=detected,size=detected),shape=21,alpha=0.7)+
  scale_fill_manual(values = c('1'='red','0'='grey'))+
  scale_size_manual(values = c('1'=2,'0'=1))+
  # formatting
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())

# facet wrap plot to see movement over time
# convert time from s to hr
whale_df$hr = whale_df$time/60/60
track_df$hr = track_df$time/60/60
det_df$hr = det_df$time/60/60

# generate time bin
tbin = seq(from = 0, to = max(whale_df$hr), by = 4)

# use cut to assign each row to a given time bin
whale_df$tbin = cut(x = whale_df$hr, breaks = tbin, include.lowest = TRUE)
track_df$tbin = cut(x = track_df$hr, breaks = tbin, include.lowest = TRUE)
det_df$tbin = cut(x = det_df$hr, breaks = tbin, include.lowest = TRUE)

# faceted plot both tracks with calls detected
ggplot()+
  # plot glider track
  geom_path(data = track_df,aes(x=x,y=y), color = 'black')+
  # plot whale track
  geom_path(data = whale_df,aes(x=x,y=y), color = 'grey')+
  # plot calls
  geom_point(data = det_df,aes(x=x_wh,y=y_wh,fill=detected,size=detected),shape=21,alpha=0.7)+
  scale_fill_manual(values = c('1'='red','0'='grey'))+
  scale_size_manual(values = c('1'=2,'0'=1))+
  # formatting
  facet_wrap(~tbin)+
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())

# multiple whales ----------------------------------------------------------

# produce whale movement model (no need to transform to kms)
set.seed(1)
whales_df = rw_sims(nrws = 10, hrs=24, nt = res, bh = 'socializing', radius = 10)

# produce glider track and convert to km
track_df = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd=51)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )

# simulate detection capabilities of glider
det_df = simulate_detections(whale_df = whales_df, track_df = track_df, det_method = 'visual')

# save
saveRDS(object = whales_df, file = ofile_whs)
message('Processed glider data saved as: ', ofile_whs)

# plot whales with calls and surfacings
ggplot()+
  geom_path(data = whales_df, aes(x=x,y=y,group=id,color=surface))+
  scale_color_manual(values = c('0'='grey', '1'='black'))+
  geom_point(data=filter(whales_df,call==1), aes(x=x,y=y), shape = 21, fill = 'red')+
  coord_equal()+
  theme_bw()

# plot both tracks (no detections)
ggplot()+
  geom_path(data = track_df, aes(x=x,y=y), color = 'blue')+
  geom_path(data = whales_df, aes(x=x,y=y, group=id), color = 'black')+
  geom_point(shape=1)+
  coord_equal()+
  theme_bw()

# group dive index and whale id
det_df$grp = paste(det_df$id, det_df$dive_index)

# plot both tracks with calls detected
ggplot()+
  # plot glider track
  geom_path(data = track_df,aes(x=x,y=y), color = 'black')+
  # plot whale track
  geom_path(data = whales_df,aes(x=x,y=y, group = id), color = 'grey')+
  # plot calls
  geom_path(data = det_df,aes(x=x_wh,y=y_wh, group = grp), color = 'blue')+
  geom_point(data = filter(det_df, detected == 1), aes(x=x_wh,y=y_wh), 
             shape = 21, fill = 'red', size = 2, alpha = 0.7)+
  # formatting
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())

# facet wrap plot to see movement over time
# convert time from s to hr
whales_df$hr = whales_df$time/60/60
track_df$hr = track_df$time/60/60
det_df$hr = det_df$time/60/60

# generate time bin
tbin = seq(from = 0, to = max(whales_df$hr), by = 4)

# use cut to assign each row to a given time bin
whales_df$tbin = cut(x = whales_df$hr, breaks = tbin, include.lowest = TRUE)
track_df$tbin = cut(x = track_df$hr, breaks = tbin, include.lowest = TRUE)
det_df$tbin = cut(x = det_df$hr, breaks = tbin, include.lowest = TRUE)

# faceted plot with both tracks and calls detected
ggplot()+
  # plot glider track
  geom_path(data = track_df,aes(x=x,y=y), color = 'black')+
  # plot whale track
  geom_path(data = whales_df,aes(x=x,y=y, group = id), color = 'grey')+
  # plot calls
  geom_path(data = det_df,aes(x=x_wh,y=y_wh, group = grp), color = 'blue')+
  geom_point(data = filter(det_df, detected == 1), aes(x=x_wh,y=y_wh), 
             shape = 21, fill = 'red', size = 2, alpha = 0.7)+
  # formatting
  facet_wrap(~tbin)+
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())
