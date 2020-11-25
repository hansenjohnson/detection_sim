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
nrwsl = c(1,3,10,25) 

# behaviors to test
bhl = c('feeding','socializing') 

# setup -------------------------------------------------------------------

source('tests/box_survey/box_survey_functions.R')

# process -----------------------------------------------------------------

# run surveys
DF = vector('list', length = length(nrwsl)*length(bhl))
cnt = 1
for(ii in seq_along(bhl)){
  for(jj in seq_along(nrwsl)){
    message('Simulating surveys with ', nrwsl[jj], ' in ',  bhl[ii], ' behavior (run ', cnt, ' of ', length(DF), ')')
    DF[[cnt]] = run_box_surveys(height=height,width=width,nrws=nrwsl[jj],n_surveys=n_surveys,bh=bhl[ii])
    cnt = cnt+1
  }
}

# combine
df = bind_rows(DF)

# save
saveRDS(df, file = 'tests/box_surveys.rds')
