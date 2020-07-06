## animation ##
# run model and animate tracks

# setup -------------------------------------------------------------------

install.packages('gifski')
install.packages('gganimate')
library(tidyverse)
library(gifski)
library(gganimate)
source('r/rw_sim.R')

# process -----------------------------------------------------------------

# run model for multiple right whales
df = rw_sims(nrws = 25, hrs = 48, bh = 'feeding', radius = 75, nt = 300)

# plot tracks
p = ggplot(data = df, aes(x=x,y=y, group=id))+
  geom_path( alpha = 0.5, color = 'blue')+
  geom_point(alpha = 0.7, shape=21, size = 2, fill = 'red')+
  coord_equal()+
  labs(x = 'Easting (km)', y = 'Northing (km)')+
  theme_bw()+
  theme(panel.grid = element_blank())

# create animation
anim = p + 
  transition_reveal(along = t)+
  ease_aes('linear')+
  ggtitle("Hours: {round(frame_along,0)}")

# render animation (might take some time)
anim
