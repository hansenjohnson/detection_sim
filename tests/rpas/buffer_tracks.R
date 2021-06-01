## buffer_tracks ##
# tests and practice with the sf package to buffering platform track lines

# setup -------------------------------------------------------------------

set.seed(123)
source('tests/rpas/box_survey_functions.R')
library(sf)
library(sp)
library(rgeos)

# process -----------------------------------------------------------------

# create plane survey track
trk = simulate_track(platform='plane',res=2.5,ymax=18,ymin=0,xmax=12,xmin=0)

# plot plane survey track
p = ggplot()+
  geom_path(data=trk,aes(x=x,y=y))+
  labs(x = 'Easting (km)', y = 'Northing (km)')+
  theme_bw()+
  theme(panel.grid = element_blank())
p

# make list of point coordinates from platform track line
points_coords <- data.frame(x=trk$x,
                            y=trk$y)

# make a spatial lines object (turn coordinates into a line)
lines_sp <- SpatialLines(list(Lines(Line(points_coords), ID=1)))
summary(lines_sp)
# plot
plot(points_coords, col="red")
plot(lines_sp, col="blue", add=TRUE)

# buffer line and plot
lines_buffer_sp <- gBuffer(lines_sp, width = 1.5)
plot(points_coords, col="red")
plot(lines_buffer_sp, border="red", lty="dashed", add=TRUE)
plot(lines_sp, col="blue", add=TRUE)

# reintegrate into functions ----------------------------------------------

# extract area of buffer (km2)
a = lines_buffer_sp@polygons[[1]]@area

# add attribute (time column) to the spatial points object
lines_df <- data.frame(time=c(trk$time))
lines_spdf <- SpatialLinesDataFrame(lines_sp, data=lines_df)
summary(lines_spdf)
