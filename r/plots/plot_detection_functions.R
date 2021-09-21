## plot_detection_functions ##
# example of generating and plotting visual and acoustic detection functions

library(tidyverse)
source('r/box_survey_functions.R')

# generate vector of ranges


# acoustic detection function
aco = tibble(
  r = seq(from = 0, to = 30, by = 0.1),
  p = detection_function(x = r, L = 1.045, x0 = 10, k = -0.3),
  platform = 'Slocum gliders',
  type = 'Acoustic'
)

# visual detection function
vis = tibble(
  r = seq(from = 0, to = 2.5, by = 0.1),
  p = detection_function(x = r, L = 1, x0 = 1, k = -4.8),
  platform = 'Aircrafts and vessels',
  type = 'Visual'
)

rpas = tibble(
  r = c(0, 0.0875, 0.175, 0.176, 1.5, 2.5),
  p = c(1,1,1,0, 0,0),
  platform = 'RPAS',
  type = 'Visual'
)

# combine
df = rbind(aco,vis,rpas)

platform_cols = c('Slocum gliders' = 'blue', 'Aircrafts and vessels' = 'red', 
                  'RPAS' = 'slategrey')

# same plot
p = ggplot(df,aes(x=r,y=p, group = platform, color = platform))+
  geom_path()+
  scale_color_manual(values = platform_cols)+
  labs(x='Range (km)', y='Probability of detection', color = ' Platforms')+
  facet_wrap(~type, scales='free_x')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

# save plot
ggsave('figures/detection_probability_curves.png', p, height = 3, width = 6, units = 'in', dpi = 300)
