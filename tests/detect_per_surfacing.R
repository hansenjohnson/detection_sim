## detect_per_surfacing ##
# example for counting visual detections to a per surfacing basis

# input -------------------------------------------------------------------

# define time resolution (s)
res = 3

# number of whales
nrws = 50

# behaviour
bh = 'socializing'

# model run time
hrs = 24

# waypoint file
waypoint_file = 'data/raw/waypoints_box.csv'

# setup -------------------------------------------------------------------

library(tidyverse)
source('r/rw_sim.R')
set.seed(123)

# process -----------------------------------------------------------------

# run whale movement model
whale_df = rw_sims(nrws = nrws, hrs=hrs, nt = res, bh = bh, radius = 10)

# make survey track, convert to km, and shift to better overlap with whales
track_df = make_track(waypoints = waypoint_file, res = res, spd = 4)%>%
  mutate(
    x=(x/1000)-5,
    y=(y/1000)-5
  )

# simulate detection capabilities of glider
det_df = simulate_detections(whale_df = whale_df, track_df = track_df, det_method = 'visual')

# count number of detections
n_det_orig = det_df %>% filter(detected==1) %>% nrow()
txt1 = paste0('Whale detected on ', round(n_det_orig/nrow(det_df)*100,2), '% of timesteps (', n_det_orig, '/', nrow(det_df), ')')

# plot original data
ggplot()+
  geom_path(data = track_df, aes(x=x,y=y), color = 'blue')+
  geom_point(data = filter(det_df,detected==0), aes(x=x_wh,y=y_wh),color='grey',alpha=0.5,size=0.25,shape=1)+
  geom_point(data = filter(det_df,detected==1), aes(x=x_wh,y=y_wh),color='black',alpha=0.7,size=2,shape=1)+
  labs(subtitle=txt1)+
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())
  
# count detections per surfacing
srf_df = det_df %>%
  mutate(detected=as.numeric(detected)) %>%
  group_by(id,dive_index) %>%
  summarize(
    time = mean(time),
    x_wh = mean(x_wh),
    y_wh = mean(y_wh),
    detected = sum(detected)
    )

# convert to binary (0,1) detection
srf_df$detected[srf_df$detected>0]=1
  
# count number of surface detections
n_det_surf = srf_df %>% filter(detected==1) %>% nrow()
txt2 = paste0('Whale detected on ', round(n_det_surf/nrow(srf_df)*100,2), '% of surfacings (', n_det_surf, '/', nrow(srf_df), ')')

# plot corrected data
ggplot()+
  geom_path(data = track_df, aes(x=x,y=y), color = 'blue')+
  geom_point(data = filter(srf_df,detected==0), aes(x=x_wh,y=y_wh),color='grey',alpha=0.5,size=0.25,shape=1)+
  geom_point(data = filter(srf_df,detected==1), aes(x=x_wh,y=y_wh),color='black',alpha=0.7,size=2,shape=1)+
  labs(subtitle=txt2)+
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())


