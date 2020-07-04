## map ##
# run model, transform to lat/lon, and plot on map

# setup -------------------------------------------------------------------

library(tidyverse)
library(oce)
source('r/rw_sim.R')

# coastline data
bg = readRDS(url('https://hansenjohnson.org/files/data/coastline.rds'))

# process -----------------------------------------------------------------

# run model for multiple right whales
df = rw_sims(nrws = 25, hrs = 48, bh = 'feeding', radius = 75)

# convert from local coords to lat/lon (distances must be in meters)
tmp = geodXyInverse(x = df$x*1e3, y = df$y*1e3, longitudeRef = -63, latitudeRef = 48)

# add to data frame
df$lat = tmp$latitude
df$lon = tmp$longitude

# plot map
ggplot()+
  geom_polygon(data = bg, aes(x = lon, y = lat, group = group), 
               fill = "darkgrey", color = NA) + 
  geom_path(data = df, aes(x=lon,y=lat, group=id), alpha = 0.5, color = 'blue')+
  coord_quickmap(xlim = c(-65, -61), ylim = c(46.5, 49))+
  labs(x = NULL, y = NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())
