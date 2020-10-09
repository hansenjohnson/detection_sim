## plot_detection_functions ##
# example of generating and plotting visual and acoustic detection functions

library(tidyverse)
source('r/rw_sim.R')

# generate vector of ranges


# acoustic detection function
aco = tibble(
  r = seq(from = 0, to = 30, by = 0.1),
  p = detection_function(x = r, L = 1.045, x0 = 10, k = -0.3),
  type = 'acoustic'
)

# visual detection function
vis = tibble(
  r = seq(from = 0, to = 3, by = 0.1),
  p = detection_function(x = r, L = 1, x0 = 1, k = -4.8),
  type = 'visual'
)

# combine
df = rbind(aco,vis)

# same plot
p1 = ggplot(df,aes(x=r,y=p,group=type,color=type))+
  geom_path()+
  labs(x='Range (km)', y='Probability', color = 'Method')+
  # scale_x_continuous(limits = c(0,30))+
  theme_bw()

# faceted
p2 = p1+facet_wrap(~type, scales = "free_x")


