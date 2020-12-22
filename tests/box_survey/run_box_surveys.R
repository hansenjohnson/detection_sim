## run_box_surveys ##
# run numerous box surveys over a range of parameters

# input -------------------------------------------------------------------

# height of box (km)
height=18             

# width of box (km)
width=12              

# number of surveys to conduct for each parameter combination
n_surveys=50          

# numbers of whales to test
nrwsl = c(1, 5, 10, 25, 50, 75) 

# numbers of surveys to test
#snl = c(10, 50) 

# setup -------------------------------------------------------------------

source('tests/box_survey/box_survey_functions.R')
library(tidyverse)
library(dplyr)
library(ggplot2)
set.seed(123)

# process -----------------------------------------------------------------

# run surveys
DF = vector('list', length = length(nrwsl))
#cnt = 1
#for(ii in seq_along(snl)){
  for(jj in seq_along(nrwsl)){
    message('Simulating surveys with ', nrwsl[jj], ' whale(s)')
    DF[[jj]] = run_box_surveys(height=height,width=width,nrws=nrwsl[jj],n_surveys=n_surveys)
    #cnt = cnt+1
  }
#}

# combine
df = bind_rows(DF)

# save
saveRDS(df, file = 'tests/box_surveys.rds')
df = readRDS('tests/box_surveys.rds')

# plot --------------------------------------------------------------------
#df$type = paste(df$platform,df$behavior)
  
# probability of detecting at least one whale with increasing numbers of whales, per platform
p = ggplot()+
  geom_path(data = df, aes(x = n_whales, y = transit_p, color = platform, group = platform))+
  labs(x = 'number of whales', y = 'probability of detection')+
  #facet_grid(~n_whales)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

# probability of detecting one whale with increasing numbers of transits, per platform
# probability of detecting a whale on a single transit
w_plane = filter(df, platform == 'plane', n_whales == 1) %>% select(transit_p)
w_vessel = filter(df, platform == 'vessel', n_whales == 1) %>% select(transit_p)
w_glider = filter(df, platform == 'glider', n_whales == 1) %>% select(transit_p)

# probability of missing a whale on a single transit
m_plane = 1-w_plane %>% as.numeric(m_plane)
m_vessel = 1-w_vessel %>% as.numeric(m_vessel)
m_glider = 1-w_glider %>% as.numeric(m_glider)

# vector of numbers of transits
n = seq(from = 1, to = 50, by = 1)

# compute probability of detection on 1 of n transits
p_plane = 1-m_plane^n
p_vessel = 1-m_vessel^n
p_glider = 1-m_glider^n

# put in tibble for plotting
probs = tibble(n,p_plane, p_vessel, p_glider)

# plot
ggplot()+
  geom_path(data= probs, aes(x=n,y=p_plane, colour = 'green'))+
  geom_path(data= probs, aes(x=n,y=p_vessel, colour = 'blue'))+
  geom_path(data= probs, aes(x=n,y=p_glider, colour = 'red'))+
  labs(x='Number of transits', y = 'Probability of detection')+
  ylim(c(0,1))+
  theme_bw()
