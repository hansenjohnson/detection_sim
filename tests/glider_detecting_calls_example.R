## glider_detecting_calls_example ##
# run through a fully-fledged example with all functions read in

# setup -------------------------------------------------------------------

source('r/rw_sim.R')

# process -----------------------------------------------------------------

# define time resolution (s)
res = 60

# produce whale movement model
whale_df = rw_sim(nt = res, sub = TRUE)

# produce glider track
track_df = make_track(res = res)

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
  geom_path(data = track_df,aes(x=x_dt,y=y_dt), color = 'black')+
  # plot whale track
  geom_path(data = whale_df,aes(x=x_wh,y=y_wh), color = 'grey')+
  # plot calls
  geom_point(data = calls,aes(x=x_wh,y=y_wh,fill=detected,size=detected),shape=21,alpha=0.7)+
  scale_fill_manual(values = c('1'='red','0'='grey'))+
  scale_size_manual(values = c('1'=2,'0'=1))+
  # formatting
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())
