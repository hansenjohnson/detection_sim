## make_box ##
# make a box (management area) and check detections inside and out

# setup -------------------------------------------------------------------

library(tidyverse)
library(sf)

make_box = function(x=0,y=0,width=90,height=25){
  # build a rectangular sf object of given width and height (in km)
  # with lower left corner at coordinates (x,y)
  
  # define corners
  bl = c(x,y)
  br = c(x+width,y)
  tr = c(x+width,y+height)
  tl = c(x,y+height)
  
  # create box
  bx = st_sfc(st_polygon(list(rbind(bl,br,tr,tl,bl))))
  
  return(bx)
}

# make box ----------------------------------------------------------------

# create a box
bx = make_box(x=-40,y=-10,width=90,height=25)

# plot box
ggplot()+
  geom_sf(data = bx)+
  theme_bw()

# read in detections
df = readRDS('data/processed/all_detections.rds') %>%
  filter(detected == 1)

# convert points to sf object
df_sf = st_as_sf(df, coords = c("x_wh", "y_wh"))

# which detections were within the box? (use `st_within`)
df$inside = as.vector(st_within(x = df_sf, y = bx, sparse = F))

# plot detections inside the box by platform
ggplot()+
  geom_sf(data = bx)+
  geom_point(data=df,aes(x=x_wh,y=y_wh,fill=inside),shape=21)+
  scale_fill_manual(values = c('TRUE'='red','FALSE'='blue'))+
  facet_wrap(~platform)+
  theme_bw()
