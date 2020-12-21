## transit_p ##
# quick example of how to calculate probability of detection by number of transits

library(tidyverse)

# probability of detecting a whale on a single transit
w = 0.25

# probability of missing a whale on a single transit
m = 1-w

# vector of numbers of transits
n = seq(from = 1, to = 25, by = 1)

# compute probability of detection on 1 of n transits
p = 1-m^n

# put in tibble for plotting
df = tibble(n,p)

# plot
ggplot(df,aes(x=n,y=p))+
  geom_path()+
  labs(x='Number of transits', y = 'Probability of detection')+
  ylim(c(0,1))+
  theme_bw()
