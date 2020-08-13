## glider_detecting_calls_example ##
# run through a fully-fledged example with all functions read in

# setup -------------------------------------------------------------------

source('r/rw_sim.R')

# process -----------------------------------------------------------------

# define time resolution (s)
res = 60

# produce whale movement model and convert to km
whale_df = rw_sim(hrs=24*5, nt = res, sub = TRUE, x0 = 5000, y0 = 5000, bh = 'feeding', cr_mn_hr = 5)%>%
  mutate(
    x=x/1000,
    y=y/1000,
    r=r/1000
  )

# produce glider track and convert to km
track_df = make_track(res = res)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )

# simulate detection capabilities of glider
det_df = simulate_detections(whale_df = whale_df, track_df = track_df)

# plot --------------------------------------------------------------------

# plot whale with calls
ggplot()+
  geom_path(data = whale_df,aes(x=x,y=y))+
  geom_point(data = filter(whale_df,call==1),aes(x=x,y=y),shape=21,fill='red')+
  coord_fixed()

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
tbin = seq(from = 0, to = max(whale_df$hr), by = 12)

# use cut to assign each row to a given time bin
whale_df$tbin = cut(x = whale_df$hr, breaks = tbin, include.lowest = TRUE)
track_df$tbin = cut(x = track_df$hr, breaks = tbin, include.lowest = TRUE)
det_df$tbin = cut(x = det_df$hr, breaks = tbin, include.lowest = TRUE)

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
  facet_wrap(~tbin)+
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())
