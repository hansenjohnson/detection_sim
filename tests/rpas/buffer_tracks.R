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

# set up survey box area
ymax = 18
ymin = 0
xmax = 12
xmin = 0

# create plane survey track
trk = simulate_track(platform='plane',res=2.5,ymax=ymax,ymin=ymin,xmax=xmax,xmin=xmin)

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

# extract area of buffer (km2)
a = lines_buffer_sp@polygons[[1]]@area
track_area = tibble(a)

# find extent of survey box and crop crop buffer to within box
ext = extent(xmin, xmax, ymin, ymax)
crop(x = lines_buffer_sp, y = ext)

# function ----------------------------------------------------------------

# create plane survey track
trk = simulate_track(platform='plane',res=2.5,ymax=18,ymin=0,xmax=12,xmin=0)

# combine into function
calculate_buffer = function(trk, platform, xmin, xmax, ymin, ymax, plot_check = FALSE){
  
  # assign buffer based on platform (~50% of platform total range, km)
  if(platform == 'glider'){
    bdist = 20 
  } else if (platform == 'plane'){
    bdist = 1.9 
  } else if (platform == 'vessel'){
    bdist = 1.9
  } else if (platform == 'rpas'){
    bdist = 0.088 
  } else {
    stop('Platform not recognized!')
  }
  
  # make list of point coordinates from platform track line
  points_coords = data.frame(x=trk$x, y=trk$y)
  
  # make a spatial lines object (turn coordinates into a line)
  lines_sp = SpatialLines(list(Lines(Line(points_coords), ID=1)))

  # buffer line
  lines_buffer_sp = gBuffer(lines_sp, width = bdist)
  
  # find extent of survey box and crop crop buffer to within box
  ext = extent(xmin, xmax, ymin, ymax)
  lines_buffer_sp_cropped = crop(x = lines_buffer_sp, y = ext)
  
  # extract area of buffer (km2)
  a = lines_buffer_sp_cropped@polygons[[1]]@area
  track_area = tibble(a)

  if(plot_check){
    plot(ext)
    plot(lines_buffer_sp, border="red", lty="dashed", add=TRUE)
    plot(lines_buffer_sp_cropped, border="green", lwd = 3, add=TRUE)
  }
  
  return(track_area)
}

# test function
area = calculate_buffer(trk, platform = 'plane', xmin=0, xmax=12, ymin=0, ymax=18, plot_check = T)
