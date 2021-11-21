## plot_box_survey_examples ##
# simulate and plot data from a few surveys

# input -------------------------------------------------------------------

# parameters
nrws = 5
n_surveys = 3
height = 18
width = 12

# setup -------------------------------------------------------------------

# read in functions
source('r/box_survey_functions.R')

# make plot reproducible
set.seed(1)

# process -----------------------------------------------------------------

# generate example data
gld = box_surveys(height = height, width = width, platform = 'slocum', nrws = nrws, n_surveys = n_surveys)
ves = box_surveys(height = height, width = width, platform = 'vessel', nrws = nrws, n_surveys = n_surveys)
pln = box_surveys(height = height, width = width, platform = 'plane', nrws = nrws, n_surveys = n_surveys)
rpas = box_surveys(height = height, width = width, platform = 'rpas', nrws = nrws, n_surveys = n_surveys)

# combine
df = rbind(gld, ves, pln, rpas)

# change platform names for plotting
df$platform = recode(df$platform, slocum = "Slocum glider (\u0394t = 39 hr)", plane = "Aircraft (\u0394t = 5 mins)", 
                     vessel = "Vessel (\u0394t = 1 hr)", rpas = "RPAS (\u0394t = 6 mins)")

# define platform factor for plotting order
df$platform = factor(df$platform, levels = c("Aircraft (\u0394t = 5 mins)",
                                             "RPAS (\u0394t = 6 mins)",
                                             "Vessel (\u0394t = 1 hr)",
                                             "Slocum glider (\u0394t = 39 hr)"), ordered = TRUE)

# extract plotting data
whale_df = filter(df,run=="1") %>% 
  dplyr::select(run, platform, whale_df) %>%
  unnest(whale_df)
track_df = filter(df,run=="1") %>% 
  dplyr::select(run, platform, track_df) %>%
  unnest(track_df)
det_df = filter(df,run=="1") %>% 
  dplyr::select(run, platform, det_df) %>%
  unnest(det_df)

# plot
p = ggplot()+
  geom_path(data=track_df,aes(x=x,y=y),color='blue')+
  geom_path(data=whale_df,aes(x=x,y=y,group=sid,color=surface))+
  geom_point(data=filter(whale_df,call==1),aes(x=x,y=y),shape=1)+
  geom_point(data=filter(det_df,detected==1), aes(x=x_wh,y=y_wh), shape = 21, fill = 'red')+
  scale_color_manual(values = c('grey','black'))+
  labs(x='Easting (km)',y='Northing (km)')+
  coord_equal(expand = F)+
  facet_grid(~platform)+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = "right")
p

# save plot
ggsave('figures/box_survey_examples.png', p, height = 7, width = 8, units = 'in', dpi = 300)
