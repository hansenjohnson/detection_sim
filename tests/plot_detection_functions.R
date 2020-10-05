## plot_detection_functions ##
# example of generating and plotting visual and acoustic detection functions

library(tidyverse)
source('r/rw_sim.R')

# generate vector of ranges
r = seq(from = 0, to = 40, by = 0.1)

# acoustic detection function
aco = tibble(
  r,
  p = detection_function(x = r, L = 1.045, x0 = 10, k = -0.3),
  type = 'acoustic'
)

# visual detection function
vis = tibble(
  r,
  p = detection_function(x = r, L = 1, x0 = 1, k = -4.8),
  type = 'visual'
)

# combine
df = rbind(aco,vis)

# plot
ggplot(df,aes(x=r,y=p,group=type,color=type))+
  geom_path()+
  labs(x='Range (km)', y='Probability', color = 'Method')+
  theme_bw()
