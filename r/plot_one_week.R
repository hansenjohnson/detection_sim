## f_1-week ##
# plot results from one week example

# input -------------------------------------------------------------------

ifile_whs = 'data/processed/multiple_whales_movement.rds'
ifile_trk = 'data/processed/platforms_movement.rds'
ifile_det = 'data/processed/all_detections.rds'

# setup -------------------------------------------------------------------

library(tidyverse)
library(gganimate)

# plot --------------------------------------------------------------------

# read in data
whs = readRDS(ifile_whs)
trk = readRDS(ifile_trk)
det = readRDS(ifile_det)

# downsample whale and track data
whs = whs[seq(from = 1, to = nrow(whs), by = 60),]
trk = trk[seq(from = 1, to = nrow(trk), by = 5),]

# format for plotting
whs = whs %>%
  transmute(time,x,y,id,day=as.numeric(day))
trk = trk %>%
  transmute(time,x,y,id,platform,day=as.numeric(day))
det = det %>% 
  filter(detected==1) %>%
  transmute(time,x=x_wh,y=y_wh,platform,day=as.numeric(day))

# plot full dataset
p = ggplot()+
  # whales
  geom_path(data = whs, aes(x=x,y=y,group=id),color='grey',size=0.3)+
  # tracks
  geom_path(data = trk, aes(x=x,y=y,group=id),color='blue')+
  # detections
  geom_point(data=det, aes(x=x,y=y), shape = 21, fill = 'red')+
  # facet
  facet_grid(~platform)+
  # formatting
  coord_equal()+
  labs(x = 'Easting (km)', y = 'Northing (km)')+
  theme_bw()+
  theme(panel.grid = element_blank())

# save full plot
ggsave(plot = p, filename = 'figures/1-week.png',
       width = 6, height = 2.5, units = 'in', dpi = 300)

# animate
anim = p + 
  transition_manual(day)+
  ggtitle("Day: {current_frame}/7")

# save animation
anim_save(animation = anim, filename = 'figures/1-week.gif', 
          nframes = 7, fps = 1, renderer = gifski_renderer(), 
          width = 6, height = 2.5, units = 'in', res = 300)
