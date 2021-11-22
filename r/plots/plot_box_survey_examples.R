## plot_box_survey_examples ##
# simulate and plot data from a few surveys

# input -------------------------------------------------------------------

# parameters
nrws = 5
height = 18
width = 12

# setup -------------------------------------------------------------------

# read in functions
source('r/box_survey_functions.R')

# make plot reproducible
set.seed(1)

# process -----------------------------------------------------------------

# generate example data

gld = box_survey(height = height, width = width, platform = 'slocum', nrws = nrws, include_data = TRUE)
ves = box_survey(height = height, width = width, platform = 'vessel', nrws = nrws, include_data = TRUE)
pln = box_survey(height = height, width = width, platform = 'plane', nrws = nrws, include_data = TRUE)
rpas = box_survey(height = height, width = width, platform = 'rpas', nrws = nrws, include_data = TRUE)

# combine
df = rbind(gld, ves, pln, rpas)

# calculate buffer --------------------------------------------------------

get_buffer = function(df, pform, bdist){
  
  # extract track
  trk = df %>% dplyr::select(platform, track_df) %>%
    unnest(track_df) %>% dplyr::filter(platform == pform)
  
  # make list of point coordinates from platform track line
  points_coords = data.frame(x=trk$x, y=trk$y)
  
  # make a spatial lines object (turn coordinates into a line)
  lines_sp = SpatialLines(list(Lines(Line(points_coords), ID=1)), 
                          proj4string = CRS("+proj=laea +lat_0=45 +lon_0=-65 +datum=WGS84 +units=km"))
  
  # buffer line
  lines_buffer_sp = gBuffer(lines_sp, width = bdist, byid = TRUE)
  
  # fortify for plotting
  trk_buffer = fortify(lines_buffer_sp)
  trk_buffer$platform = pform
  return(trk_buffer)
}

# compute
gld_buff = get_buffer(df, 'slocum', 10)
ves_buff = get_buffer(df, 'vessel', 1.5)
pln_buff = get_buffer(df, 'plane', 1.5)
rpa_buff = get_buffer(df, 'rpas', 0.172)

# combine
buff_df = rbind(gld_buff,ves_buff,pln_buff,rpa_buff)

# plot --------------------------------------------------------------------

# change platform names for plotting
df$platform = recode(df$platform, slocum = "Slocum glider (\u0394t = 39 hr)", plane = "Aircraft (\u0394t = 5 mins)", 
                     vessel = "Vessel (\u0394t = 1 hr)", rpas = "RPAS (\u0394t = 6 mins)")
buff_df$platform = recode(buff_df$platform, slocum = "Slocum glider (\u0394t = 39 hr)", plane = "Aircraft (\u0394t = 5 mins)", 
                     vessel = "Vessel (\u0394t = 1 hr)", rpas = "RPAS (\u0394t = 6 mins)")

# define platform factor for plotting order
df$platform = factor(df$platform, levels = c("Aircraft (\u0394t = 5 mins)",
                                             "RPAS (\u0394t = 6 mins)",
                                             "Vessel (\u0394t = 1 hr)",
                                             "Slocum glider (\u0394t = 39 hr)"), ordered = TRUE)
buff_df$platform = factor(buff_df$platform, levels = c("Aircraft (\u0394t = 5 mins)",
                                             "RPAS (\u0394t = 6 mins)",
                                             "Vessel (\u0394t = 1 hr)",
                                             "Slocum glider (\u0394t = 39 hr)"), ordered = TRUE)

# extract plotting data
whale_df = df %>% dplyr::select(platform, whale_df) %>%
  unnest(whale_df)
det_df = df %>% dplyr::select(platform, det_df) %>%
  unnest(det_df)
track_df = df %>% dplyr::select(platform, track_df) %>%
  unnest(track_df)

p = ggplot()+
  geom_polygon(data=buff_df,aes(x=long,y=lat,group=group),fill='blue',color = NA,alpha=0.3)+
  geom_path(data=track_df,aes(x=x,y=y), color = 'blue')+
  geom_path(data=whale_df,aes(x=x,y=y,group=sid,color=surface))+
  geom_point(data=filter(whale_df,call==1),aes(x=x,y=y),shape=1)+
  geom_point(data=filter(det_df,detected==1), aes(x=x_wh,y=y_wh), shape = 21, fill = 'red')+
  scale_color_manual(values = c('grey','black'))+
  labs(x='Easting (km)',y='Northing (km)')+
  coord_equal(expand = F, xlim = c(-6,6), ylim = c(-9,9))+
  facet_grid(~platform)+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = "right")
p

# save plot
ggsave('figures/box_survey_examples.png', p, height = 7, width = 8, units = 'in', dpi = 300)
