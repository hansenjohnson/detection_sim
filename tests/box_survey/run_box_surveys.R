## run_box_surveys ##
# run numerous box surveys over a range of parameters

# input -------------------------------------------------------------------

# height of DFO fishing area box (km)
#dfo_height=18             

# width of DFO fishing area box (km)
#dfo_width=12    

# height of TC speed area box (km)
#tc_height=20             

# width of TC speed area box (km)
#tc_width=100 

# number of surveys to conduct for each parameter combination
#n_surveys=10          

# numbers of whales to test
#nrwsl = c(1, 5, 10, 25, 50, 75) 

# numbers of surveys to test
#snl = c(10, 50) 

# setup -------------------------------------------------------------------

source('tests/box_survey/box_survey_functions.R')

library(tidyverse)
#library(dplyr)
#library(ggplot2)

set.seed(123)

# process -----------------------------------------------------------------

# run surveys for DFO fishing zones 
#DF = vector('list', length = length(nrwsl))
#cnt = 1
#for(ii in seq_along(snl)){
  #for(jj in seq_along(nrwsl)){
    #message('Simulating surveys with ', nrwsl[jj], ' whale(s)')
    #DF[[jj]] = run_box_surveys(height=dfo_height,width=dfo_width,nrws=nrwsl[jj],n_surveys=n_surveys)
    #cnt = cnt+1
  #}
#}

# combine
#df = bind_rows(DF)

# save and call
#saveRDS(df, file = 'tests/fishing_box_surveys.rds')
df_1 = readRDS('tests/fishing_box_surveys.rds')

# run surveys for TC speed restriction zones 
#DF_2 = vector('list', length = length(nrwsl))
#cnt = 1
#for(ii in seq_along(snl)){
#for(jj in seq_along(nrwsl)){
  #message('Simulating surveys with ', nrwsl[jj], ' whale(s)')
  #DF_2[[jj]] = run_box_surveys(height=tc_height,width=tc_width,nrws=nrwsl[jj],n_surveys=n_surveys)
  #cnt = cnt+1
#}
#}

# combine
#df_2 = bind_rows(DF_2)

# save and call
#saveRDS(df_2, file = 'tests/speed_box_surveys.rds')
df_2 = readRDS('tests/speed_box_surveys.rds')

# plot --------------------------------------------------------------------
#df$type = paste(df$platform,df$behavior)

# probability of detecting one whale with increasing numbers of transits, per platform
# probability of detecting a whale on a single transit
#w_plane = filter(df, platform == 'plane', n_whales == 1) %>% select(transit_p)
#w_vessel = filter(df, platform == 'vessel', n_whales == 1) %>% select(transit_p)
#w_glider = filter(df, platform == 'glider', n_whales == 1) %>% select(transit_p)

# probability of missing a whale on a single transit
#m_plane = 1-w_plane %>% as.numeric()
#m_vessel = 1-w_vessel %>% as.numeric()
#m_glider = 1-w_glider %>% as.numeric()

# vector of numbers of transits
#n = seq(from = 1, to = 25, by = 1)

# compute probability of detection on 1 of n transits
#p_plane = 1-m_plane^n
#p_vessel = 1-m_vessel^n
#p_glider = 1-m_glider^n

# put in tibble for plotting
#probs = tibble(n,p_plane, p_vessel, p_glider)

# plot
#ggplot()+
  #geom_path(data= probs, aes(x=n,y=p_plane, colour = 'green'))+
  #geom_path(data= probs, aes(x=n,y=p_vessel, colour = 'blue'))+
  #geom_path(data= probs, aes(x=n,y=p_glider, colour = 'red'))+
  #labs(x='Number of transits', y = 'Probability of detection')+
  #ylim(c(0,1))+
  #theme_bw()

# alternative -------------------------------------------------------------

# calculate groupwise transit probabilities 
#probs2 = df %>%
  #group_by(platform,n_whales) %>%
  #summarize(
    #n = seq(from = 1, to = 25, by = 1),
    #p = 1-(1-transit_p)^n
  #)

# plot with number of whales in each facet
#ggplot()+
  #geom_path(data=probs2,aes(x=n,y=p,color=platform))+
  #facet_wrap(~n_whales)+
  #labs(x='Number of transits', y = 'Probability of detection')+
  #ylim(c(0,1))+
  #theme_bw()

# functions -----------------------------------------------------------------

box_whales_surveys = function(height = 18, # box height (km), default is dfo
                              width = 12, # box width (km), default is dfo
                              n_surveys = 10, # number of times the survey is run
                              nrwsl = c(1, 5, 10, 25, 50, 75) # numbers of whales to test
){
  
  # run surveys for DFO box 
  DF = vector('list', length = length(nrwsl))

  for(jj in seq_along(nrwsl)){
    message('Simulating surveys with ', nrwsl[jj], ' whale(s)')
    DF[[jj]] = run_box_surveys(height=height,width=width,nrws=nrwsl[jj],n_surveys=n_surveys)
  }
  message('Done! Box is ', height, ' km high and ', width, ' km wide')
  
  # combine
  df = bind_rows(DF)
  
  # add column for box type
  if(height == 18 | width == 12){
    df$box_type = 'DFO'
  } else if (height == 20 | width == 100){
    df$box_type = 'TC'
  } else {
    df$box_type = 'other'
  }
  
  return(df)
}

# plot -----------------------------------------------------------------

df_1 = box_whales_surveys(nrwsl = c(1,5,10,25,50))
df_2 = box_whales_surveys(height = 20,width = 100,nrwsl = 1)

# combine
all_boxes = bind_rows(df_1, df_2)

# calculate groupwise whale number probabilities 
probs_whale = all_boxes %>%
  group_by(platform,n_whales,box_type)
  
# calculate groupwise transit probabilities 
probs_transit = all_boxes %>%
  group_by(platform,n_whales,box_type) %>%
  summarize(
    n = seq(from = 1, to = 25, by = 1),
    p = 1-(1-transit_p)^n
  )

df = readRDS('data/processed/box_surveys.rds')

# probability of detecting at least one whale with increasing numbers of whales, 
# per platform and box type
ggplot()+
  geom_path(data = tc, aes(x = n_whales, y = transit_p, color = platform))+
  labs(x = 'number of whales', y = 'probability of detection')+
  #facet_grid(~box_type)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# probability of detecting whales with increasing numbers of transits, 
# per platform and box type
probs = df %>%
  group_by(platform,n_whales,box_type) %>%
  summarize(
    n = seq(from = 1, to = 25, by = 1),
    p = 1-(1-transit_p)^n
  ) %>% filter(n_whales %in% c(1,10,20))

ggplot()+
  geom_path(data=probs,aes(x=n,y=p,color=platform, linetype = box_type))+
  facet_wrap(~n_whales)+
  labs(x='Number of transits', y = 'Probability of detection')+
  ylim(c(0,1))+
  theme_bw()
