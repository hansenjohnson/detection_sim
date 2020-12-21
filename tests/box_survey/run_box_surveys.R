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

# plot --------------------------------------------------------------------
#df$type = paste(df$platform,df$behavior)
  
p = ggplot()+
  geom_path(data = df, aes(x = n_whales, y = transit_p, color = platform, group = platform))+
  labs(x = 'number of whales', y = 'probability of detection')+
  #facet_grid(~n_whales)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p
