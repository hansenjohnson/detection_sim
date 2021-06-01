## buffer_tracks ##
# tests and practice with the sf package to buffering platform track lines

# setup -------------------------------------------------------------------

set.seed(123)
source('tests/rpas/box_survey_functions.R')
library(sf)
library(sp)
library(rgeos)
library(raster)

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

ext = extent(xmin, xmax, ymin, ymax)
crop(x = lines_buffer_sp, y = ext)
# function ----------------------------------------------------------------

# create plane survey track
trk = simulate_track(platform='plane',res=2.5,ymax=18,ymin=0,xmax=12,xmin=0)

calculate_buffer = function(trk, platform, xmin, xmax, ymin, ymax, plot_check = FALSE){
  
  # assign buffer based on platform (km)
  if(platform == 'glider'){
    bdist = 20 
  } else if (platform == 'plane'){
    bdist = 2 
  } else if (platform == 'vessel'){
    bdist = 2
  } else if (platform == 'rpas'){
    bdist = 0.2 
  } else {
    stop('Platform not recognized!')
  }
  
  # make list of point coordinates from platform track line
  points_coords = data.frame(x=trk$x, y=trk$y)
  
  # make a spatial lines object (turn coordinates into a line)
  lines_sp = SpatialLines(list(Lines(Line(points_coords), ID=1)))

  # buffer line and plot
  lines_buffer_sp = gBuffer(lines_sp, width = bdist)
  
  # crop to within box
  ext = extent(xmin, xmax, ymin, ymax)
  lines_buffer_sp_cropped = crop(x = lines_buffer_sp, y = ext)
  
  # extract area of buffer (km2)
  a = lines_buffer_sp_cropped@polygons[[1]]@area
  
  if(plot_check){
    plot(ext)
    plot(lines_buffer_sp, border="red", lty="dashed", add=TRUE)
    plot(lines_buffer_sp_cropped, border="green", lwd = 3, add=TRUE)
  }
  
  return(a)
}

calculate_buffer(trk, platform = 'plane', ymax=18, ymin=0, xmax=12, xmin=0, plot_check = T)
