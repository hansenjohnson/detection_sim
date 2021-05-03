## new_test_functions ##
# trying new functions where platforms have different detection ranges and capabilities

# input -------------------------------------------------------------------

# input parameters
x=seq(from = 0, to = 200, by = 0.5)
y = 1.00

# put in table
df = tibble(x=x,y=y)

# plot
ggplot()+
  geom_point(data=df,aes(x=x,y=y))

# select only those data points up to 175
detections = df %>% filter(x <= 175)

# plot
ggplot()+
  geom_point(data=detections,aes(x=x,y=y))

# setup -------------------------------------------------------------------

# libraries
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(zoo))

source('r/box_survey_functions.R')

# functions ---------------------------------------------------------------

new_detection_function = function(x,L=1.045,x0=10,k=-0.3,y=y){
  # Construct a detection function using a logistic curve
  # L = maximum Y value
  # x0 = value at midpoint
  # k = logistic growth rate
  y = L/(1+exp(-1*k*(x-x0))) 
    
  return(y)
}
  
new_simulate_detections = function(whale_df = wh, track_df = trk, 
                                   det_method = 'acoustic', platform = 'glider'){
  # simulate detections of whales (from rw_sim) by a platform following a survey track (from simulate_track)
  
  # deal with missing id column for single whale
  if(!"id" %in% colnames(whale_df)){
    whale_df$id = 1
  }
  
  #rename whale movement model table and lose unwanted variables
  whale_df = whale_df %>% transmute(id, x_wh=x, y_wh=y, time, call, dive_index, surface)
  
  #rename track movement model table 
  track_df = track_df %>% transmute(x_dt=x, y_dt=y, time)
  
  # make data frame using whale movement variables
  df = left_join(whale_df, track_df, by='time', all.x=TRUE)
  df$r_wh = sqrt((df$x_wh-df$x_dt)^2 + (df$y_wh-df$y_dt)^2)
  
  if(det_method == 'acoustic'){
    # subset to only times with calls
    detections = df %>% filter(call==1) %>% select(-x_dt, -y_dt, -dive_index, -surface)
    # apply detection function to the call positions to extract probabilities of detection
    detections$p = new_detection_function(x = detections$r_wh, L = 1.045, x0 = 10, k = -0.3)
  } else {
    # subset to only times with whale at the surface
    detections = df %>% filter(surface==1) %>% select(-x_dt, -y_dt, -call)
    
    if(platform == 'plane'){
      # apply detection function to the surfacing positions to extract probabilities of detection
      detections$p = new_detection_function(x = detections$r_wh, L = 1, x0 = 1, k = -4.8)
    }
    else if (platform == 'vessel'){
      # apply detection function to the surfacing positions to extract probabilities of detection
      detections$p = new_detection_function(x = detections$r_wh, L = 1, x0 = 1, k = -4.8)
    }
    else if (platform == 'rpas'){
      # apply detection function to the surfacing positions to extract probabilities of detection
      detections$p = new_detection_function(x = (detections %>% filter(r_wh <= 175)), y=1)
    }
  }
  
  # generate a binomial distribution to see if each call/surfacing was detected using this probability
  detections$detected = as.character(rbinom(n = nrow(detections), size = 1, prob = detections$p))
  
  # remove NAs
  detections = detections[complete.cases(detections),]
  
  return(detections)
}

new_run_box_surveys = function(height = 18,
                           width = 12,
                           n_surveys = 10,
                           n_whales = 10,
                           bh = 'feeding',
                           whales_parallel = FALSE,
                           survey_parallel = TRUE
) {
  
  # record start time
  tic = Sys.time()
  
  # set up progress bar
  pb = txtProgressBar(min = 0, max = length(n_whales), style = 3)
  
  # run surveys
  DF = vector('list', length = length(n_whales))
  for (ii in seq_along(n_whales)) {
    
    # run surveys for each platform
    gld = box_surveys(
      platform = 'glider',
      height = height,
      width = width,
      nrws = n_whales[ii],
      n_surveys = n_surveys,
      bh = bh,
      whales_parallel = whales_parallel,
      survey_parallel = survey_parallel,
      include_data = FALSE
    )
    pln = box_surveys(
      platform = 'plane',
      height = height,
      width = width,
      nrws = n_whales[ii],
      n_surveys = n_surveys,
      bh = bh,
      whales_parallel = whales_parallel,
      survey_parallel = survey_parallel,
      include_data = FALSE
    )
    ves = box_surveys(
      platform = 'vessel',
      height = height,
      width = width,
      nrws = n_whales[ii],
      n_surveys = n_surveys,
      bh = bh,
      whales_parallel = whales_parallel,
      survey_parallel = survey_parallel,
      include_data = FALSE
    )
    rpas = box_surveys(
      platform = 'rpas',
      height = height,
      width = width,
      nrws = n_whales[ii],
      n_surveys = n_surveys,
      bh = bh,
      whales_parallel = whales_parallel,
      survey_parallel = survey_parallel,
      include_data = FALSE
    )
    
    # combine and store
    DF[[ii]] = bind_rows(gld, pln, ves, rpas)
    
    # update progress bar
    setTxtProgressBar(pb, ii)
  }
  
  # combine
  df = bind_rows(DF)
  
  # close progress bar
  close(pb)
  
  # calculate time elapsed
  toc = round(Sys.time()-tic, 2)
  message('Done!')
  message('Time elapsed: ', format(toc))
  
  return(df)
}

# dimensions of DFO box (km)
dfo_height = 18
dfo_width = 12

# run DFO simulation
message('Running DFO simulation')
dfo = new_run_box_surveys(
  height = dfo_height,
  width = dfo_width,
  n_surveys = 10,
  n_whales = 10,
  whales_parallel = FALSE,
  survey_parallel = TRUE
) %>%
  mutate(box_type = 'DFO')
