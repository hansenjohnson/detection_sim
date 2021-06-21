## plot_detection_functions ##
# example of generating and plotting visual and acoustic detection functions

library(tidyverse)
source('r/box_survey_functions.R')

# generate vector of ranges


# acoustic detection function
aco = tibble(
  r = seq(from = 0, to = 30, by = 0.1),
  p = detection_function(x = r, L = 1.045, x0 = 10, k = -0.3),
  type = 'Slocum gliders'
)

# visual detection function
vis = tibble(
  r = seq(from = 0, to = 30, by = 0.1),
  p = detection_function(x = r, L = 1, x0 = 1, k = -4.8),
  type = 'Aircrafts and vessels'
)

rpas = tibble(
  r = seq(from = 0, to = 0.175, by = 0.1),
  p = 1,
  type = 'RPAS'
)

# combine
df = rbind(aco,vis,rpas)

# same plot
p1 = ggplot(df,aes(x=r,y=p,group=type,color=type))+
  geom_path()+
  labs(x='Range (km)', y='Probability of detection', color = ' Platforms')+
  # scale_x_continuous(limits = c(0,30))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())
# save plot
ggsave('figures/detection_probability_curves.png', p1, height = 3, width = 6, units = 'in', dpi = 300)

# faceted
p2 = p1+facet_wrap(~type, scales = "free_x")

# save plot
ggsave('figures/detection_curves_faceted.png', p2, height = 3, width = 6, units = 'in', dpi = 300)


