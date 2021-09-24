## run_box_surveys ##
# run box surveys and save output

# input -------------------------------------------------------------------

# number of surveys to run for each combination of platform, n_whale, etc.
n_surveys = 10

# numbers of right whales in box
n_whales = c(1,5,15,30)

# dimensions of box (km)
height = 18
width = 12

# setup -------------------------------------------------------------------

set.seed(123)
# read in functions
source('tests/call_distribution/detection_sim.R')

calc_call_rate = function(d){
  
  # get number of right whales
  rw = length(unique(d$id))
  
  # select calls
  n_calls = d %>%
    filter(call==1) %>%
    nrow()
  
  # calculate the observed call rate
  cr_obs = n_calls/((max(d$time))/60/60)/rw
  
  # print message
  message('Observed call rate: ', round(x = cr_obs, digits = 2), ' calls/whale/hr')
  
  return(cr_obs)
}

calc_p_detection = function(d){
  
  p_detection = sum(d$detected) / nrow(d)
  
  # print message
  message('Probability of detection: ', round(x = p_detection, digits = 2))
  
  return(p_detection)
}

# verify call rates -------------------------------------------------------

# normal
d_normal = rw_sims(nrws = 10,          
                   hrs = 48,           
                   bh = 'feeding',     
                   dt = 2.5,
                   xmin = 0,                
                   xmax = 12,               
                   ymin = 0,                
                   ymax = 18,    
                   call_dist_type = 'normal') %>%
  calc_call_rate()

# exponential
d_exponential = rw_sims(nrws = 10,          
                        hrs = 48,           
                        bh = 'feeding',     
                        dt = 2.5,
                        xmin = 0,                
                        xmax = 12,               
                        ymin = 0,                
                        ymax = 18,               
                        call_dist_type = 'exponential') %>%
  calc_call_rate()

# logistic
d_logistic = rw_sims(nrws = 10,          
                     hrs = 48,           
                     bh = 'feeding',     
                     dt = 2.5,
                     xmin = 0,                
                     xmax = 12,               
                     ymin = 0,                
                     ymax = 18,             
                     call_dist_type = 'logistic') %>%
  calc_call_rate()

# fixed
d_fixed = rw_sims(nrws = 10,          
                  hrs = 48,           
                  bh = 'feeding',     
                  dt = 2.5,
                  xmin = 0,                
                  xmax = 12,               
                  ymin = 0,                
                  ymax = 18,               
                  call_dist_type = 'fixed') %>%
  calc_call_rate()

# check detection results -------------------------------------------------

df_normal = box_surveys(platform = 'slocum',
                        height = 18,
                        width = 12,
                        nrws = 1,
                        n_surveys = 50,
                        bh = 'feeding',
                        call_dist_type = 'normal') %>%
  calc_p_detection()

df_exponential = box_surveys(platform = 'slocum',
                             height = 18,
                             width = 12,
                             nrws = 1,
                             n_surveys = 50,
                             bh = 'feeding',
                             call_dist_type = 'exponential') %>%
  calc_p_detection()

df_logistic = box_surveys(platform = 'slocum',
                          height = 18,
                          width = 12,
                          nrws = 1,
                          n_surveys = 50,
                          bh = 'feeding',
                          call_dist_type = 'logistic') %>%
  calc_p_detection()

df_fixed = box_surveys(platform = 'slocum',
                       height = 18,
                       width = 12,
                       nrws = 1,
                       n_surveys = 50,
                       bh = 'feeding',
                       call_dist_type = 'fixed') %>%
  calc_p_detection()
