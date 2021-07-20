## f_site_map ##
# site map of southern GSL

# input -------------------------------------------------------------------

# define focus of map limits (should be consistent with f_effort)
fmin_lat = 47
fmin_lon = -65.25
fmax_lat = 48.75
fmax_lon = -62.75

# setup -------------------------------------------------------------------

install.packages('rnaturalearthdata')
install.packages('ggspatial')
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(ggspatial)
library(sf)
load('data/processed/map.rda')

#  plot -------------------------------------------------------------------

# text size
sz = 3

# plot inset map
bg_inset = ne_countries(scale = "medium", returnclass = "sf")
p_inset = ggplot()+
  geom_sf(data = bg_inset,fill = "cornsilk", color = "cornsilk4", size = 0.2)+
  geom_rect(aes(xmin=min_lon,xmax=max_lon,ymin=min_lat,ymax=max_lat), color = 'black', fill = NA)+
  annotate('rect', xmin = -100, xmax = -50, ymin = 0, ymax = 55, fill = NA, color = 'black', size = 1)+
  coord_sf(xlim = c(-100,-50), ylim = c(0,55), expand = FALSE)+
  theme_void()

# plot map
p = ggplot()+
  
  # bathymetry
  geom_contour_filled(data=bf, aes(x=x,y=y,z=z), color = NA,
                      breaks = c(seq(from = 0,to = 150, by = 25), 250, 550))+
  # scale_fill_grey(start = 0.9, end = 0.3)+
  scale_fill_brewer(palette = 'Blues') +

  # dynamic management zones
  annotate('rect', xmin = -62.8, xmax = -62.6, ymin = 48.1, ymax = 48.3, 
           color = 'orangered4', linetype = 1, fill = 'orangered') +
  annotate('polygon', x = c(-65,-63.9,-63.9,-65), y = c(49.3,49.1,49.3,49.7), 
           color = 'chartreuse4', linetype = 1, fill = 'chartreuse3', alpha=.5) +
  annotate('polygon', x = c(-63.9,-62.9,-62.9,-63.9), y = c(49.1,48.8,49,49.3), 
           color = 'chartreuse4', linetype = 1, fill = 'chartreuse3', alpha=.5) +
  annotate('polygon', x = c(-62.9,-62,-62,-62.9), y = c(49,48.6,48.4,48.8), 
           color = 'chartreuse4', linetype = 1, fill = 'chartreuse3') +
  annotate('polygon', x = c(-62,-60.9,-61,-61.05,-61.1,-62), y = c(48.6,48.2,48,47.9,48.04,48.4), 
           color = 'chartreuse4', linetype = 1, fill = 'chartreuse3', alpha=.5) +
  annotate('polygon', x = c(-64,-63,-63,-64), y = c(50.3,50.3,49.9,50), 
           color = 'chartreuse4', linetype = 1, fill = 'chartreuse3',alpha=.5) +
  
  # coastline
  geom_sf(data = cf,fill = "cornsilk", color = "cornsilk4", size = 0.1)+

  # ocean labels
  geom_text(aes(x = -63.15, y = 48.3, label = 'DFO', angle = 0), color = 'black', size = sz)+
  geom_text(aes(x = -63.2, y = 48.95, label = 'TC', angle = 0), color = 'black', size = sz)+
  geom_text(aes(x = -64.5, y = 49.4, label = 'Honguedo Strait', angle = -25), color = 'white', size = sz)+
  geom_text(aes(x = -61, y = 48.5, label = 'Laurentian Channel', angle = 0), color = 'white', size = sz)+
  geom_text(aes(x = -64, y = 50.05, label = 'Jacques-Cartier Strait', angle = -5), size = sz)+
  geom_text(aes(x = -64.15, y = 47.7, label = 'Shediac Valley', angle = 60), size = sz)+
  geom_text(aes(x = -64.6, y = 48.15, label = 'Chaleur Bay', angle = 40), size = sz)+
  geom_text(aes(x = -59.8, y = 47.5, label = 'Cabot Strait', angle = 0), color = 'white', size = sz)+
  geom_text(aes(x = -66.2, y = 49.7, label = 'St Lawrence\nEstuary', angle = 0), color = 'white', size = sz)+
  geom_segment(aes(x = -66.2, y = 49.5, xend = -66.6, yend = 49.45), color = 'white', arrow = arrow(type = 'closed', length = unit(3, 'points')))+
  geom_text(aes(x = -58.8, y = 50, label = 'Strait of\nBelle Isle', angle = 0), color = 'white', size = sz)+
  geom_segment(aes(x = -58.8, y = 50.15, xend = -58.55, yend = 50.3), color = 'white', arrow = arrow(type = 'closed', length = unit(3, 'points')))+
  
  # land labels
  geom_text(aes(x = -62.95, y = 49.45, label = 'Anticosti Island', angle = -28), color = 'darkslategrey', size = sz)+
  geom_text(aes(x = -63, y = 50.75, label = 'Québec', angle = 0), color = 'darkslategrey', size = sz)+
  geom_text(aes(x = -66, y = 48.6, label = 'Québec', angle = 0), color = 'darkslategrey', size = sz)+
  geom_text(aes(x = -58.7, y = 47.9, label = 'Newfoundland', angle = 0), color = 'darkslategrey', size = sz)+
  geom_text(aes(x = -61, y = 46.3, label = 'Nova Scotia', angle = 50), color = 'darkslategrey', size = sz)+
  geom_text(aes(x = -63.45, y = 46.3, label = 'PEI', angle = 0), color = 'darkslategrey', size = sz)+
  geom_text(aes(x = -66.1, y = 47.2, label = 'New Brunswick', angle = 0), color = 'darkslategrey', size = sz)+
  geom_text(aes(x = -61.8, y = 47.5, label = 'Magdalen\nIslands', angle = 0), color = 'darkslategrey', size = sz)+
  
  # inset
  annotation_custom(grob = ggplotGrob(p_inset),
                    xmin = max_lon,
                    xmax = max_lon+2,
                    ymin = max_lat-1.25,
                    ymax = max_lat)+

  # formatting
  coord_sf(expand = FALSE, clip = 'off')+
  labs(x = NULL, y = NULL, fill = 'Depth (m)')+
  theme_bw()+
  annotation_scale(location = 'bl') +
  theme(legend.position = "right",
        legend.key = element_rect(color = 'black', fill = NA),
        panel.grid = element_blank())

p

# save
ggsave(
  filename = 'figures/site_map.png',
  plot = p,
  width = 8,
  height = 6,
  units = 'in',
  dpi = 300
)
