## rw_sim ##
# functions for right whale movement simulation

# setup -------------------------------------------------------------------

# libraries
library(tidyverse)
library(parallel)
library(oce)
library(zoo)

# functions ---------------------------------------------------------------

deg2rad = function(deg){
  # convert degrees to radians
  rad = deg*pi/180
  return(rad)
}

rad2deg = function(rad){
  # convert radians to degrees
  deg = rad*180/pi
  return(deg)
}

rw_sim = function(
  # simulate right whale movement with vectorized correlated random walk
  nrws = 1,       # number of whales in simulation
  hrs = 24,       # number of hours
  dt = 2.5,       # time resolution [sec]
  x0 = 0,         # initial x position
  y0 = 0,         # initial y position
  bh = 'feeding', # behaviour (feeding, traveling, socializing)
  nt = 60,        # new time resolution after subsampling [sec]
  sub = TRUE,     # subsample data to new rate, nt
  cr_mn_hr = 0.25,# mean call rate (calls/whale/hr)
  cr_sd_hr = 0.001# standard deviation of call rate
  )
{ 
  
  # define turn rate (in deg/10m)
  if(bh == 'feeding'){
    tr = 19.3  
  } else if(bh == 'traveling'){
    tr = 5.3   
  } else if(bh == 'socializing'){
    tr = 52.5
  } else if(bh == 'random'){
    tr = 360
  } else if(bh == 'linear'){
    tr = 0
  } else {
    return('Behaviour not recognized!')
  }
  
  # create time vector
  time = seq(from = 0, to = hrs*60*60, by = dt)
  # create a sequence from 00 to 24 with 2.5 second jumps
  
  # length time vector
  n = length(time)
  
  # calculate speeds
  spd = runif(min = 0, max = 1.23, n = n) 
  # create a uniform distribution of speeds from 0-1.23 with an observation for every jump in the time vector
  
  # calculate travel distances
  dst = spd*dt # speed * time equals distance at each time step
  dpt = cumsum(dst) # add up all the distances
  
  # calculate turn angles
  max_ang = dst*deg2rad(tr)/10 # distance travelled at a time step * turn rate for a specific behaviour 
  # (divided by 10) because tr units are given for 10m
  ang = runif(min = -max_ang, max = max_ang, n=n-1)
  
  # choose starting angle and add to rest
  ang = cumsum(c(runif(n = 1, min = 0, max = 2*pi), ang))
  
  # wrap turning angles
  ang = (ang + (2*pi)) %% (2*pi)
  
  # y movement
  y = c(y0, y0+cumsum(dst*sin(ang)))
  
  # x movement
  x = c(x0, x0+cumsum(dst*cos(ang)))
  
  # combine into data frame
  df = tibble(x = x[1:n], y = y[1:n], time, ang = rad2deg(ang), spd, dst, dpt)
  
  # downsample data
  if(sub){
    df = df[seq(from = 1, to = nrow(df), by = round(nt/dt)),]
  }
  
  # calculate range from center
  df$r = sqrt(df$x^2 + df$y^2)
  
  # add behaviour
  df$bh = bh
  
  # add call rate from normal distribution
  # calculate the time interval
  dt = df$time[2] - df$time[1]
  
  # generate a normal distribution to assign call rate in each timestep
  cr_hr = rnorm(n = nrow(df), mean = cr_mn_hr, sd = cr_sd_hr)

  # convert from hours to seconds
  cr_sec = cr_hr/60/60
  
  # calculate likelihood of call in timestep
  cr_p = cr_sec*dt # KEY - this must be less than 1!
  
  # warn if cr_p is NOT less than 1
  if(max(cr_p)>1){
    warning('Liklihood of call in timestep exceeds 1! Increase the time resolution or decrease the call rate to avoid errors')
  }
  
  # generate a binomial distribution using this probability
  df$call = rbinom(n = nrow(df), size = 1, prob = cr_p)
  
  # calculate the number of calls
  n_calls = df %>% 
    filter(call==1) %>%
    nrow()
  
  # calculate the observed call rate
  cr_obs = n_calls/hrs
  
  # print a message comparing observed and expected
  if(nrws==1){
    message('Expected call rate: ', cr_mn_hr, ' calls/whale/hr')
    message('Observed call rate: ', round(x = cr_obs, digits = 2), ' calls/whale/hr')
  }
    
  return(df)
}

init_whales = function(nrws=1e3, radius = 1e4){
  # initialize field of simulated whales within a given radius (m)
  # dropping the points (assigns an angle between 0-2pi based on a specific radius and transforms that into range)
  
  # calculate angles and ranges
  a = 2*pi*runif(n = nrws, min = 0, max = 1)
  r = radius*sqrt(runif(n = nrws, min = 0, max = 1))
  
  # convert to xy
  x=r*cos(a)
  y=r*sin(a)
  
  # return data
  out = data.frame(x=round(x),y=round(y))
}

rw_sims = function(nrws = 1e2,          # number of whales in simulation
                   hrs = 48,            # duration of simulation (hours)
                   bh = 'feeding',      # behaviour
                   nt = 300,            # model time resolution (seconds)
                   radius = 100,        # radius of initial positions (km)
                   run_parallel = FALSE,# run with parallel processing?
                   cr_mn_hr = 0.25      # mean call rate (calls/whale/hr)
){
  # simulate movements of a field of numerous whales
  
  # startup message
  message('\n## NARW MOVEMENT MODEL RUN ##\n')
  message('Input parameters:')
  message('   number of whales: ', nrws)
  message('   number of hours: ', hrs)
  message('   time resolution [sec]: ', nt)
  message('   number of timesteps: ', hrs*60*60/nt)
  message('   movement type: ', bh)
  message('Simulating whale movements...')
  
  # initialize data values
  nseq = seq(from = 1, to = nrws, by = 1)
  tic = Sys.time()
  
  # initial starting positions
  ini = init_whales(nrws = nrws, radius = radius*1e3)
  
  if(run_parallel){
    # determine number of cores available to run function more efficiently
    numCores = detectCores()
    
    message('Running in parallel with ', numCores, ' cores...')
    
    # model movements
    DF = mclapply(X = nseq, FUN = function(i){
      rw_sim(x0=ini$x[i],y0=ini$y[i],hrs=hrs,bh=bh,nt=nt,cr_mn_hr=cr_mn_hr)
    }, mc.cores = numCores)
    
  } else {
    
    # model movements
    DF = lapply(X = nseq, FUN = function(i){
      rw_sim(x0=ini$x[i],y0=ini$y[i],hrs=hrs,bh=bh,nt=nt,nrws=nrws)
    })
    
  }
  
  # flatten list to combine all whales
  df = bind_rows(DF, .id = 'id')
  
  # convert time to hours
  df$time = df$time/60/60
  
  # convert distance to kilometers
  df$x = df$x/1e3
  df$y = df$y/1e3
  df$r = df$r/1e3
  df$dst = df$dst/1e3
  df$dpt = df$dpt/1e3
  
  # calculate the number of calls
  n_calls = df %>% 
    filter(call==1) %>%
    nrow()
  
  # calculate the observed call rate
  cr_obs = n_calls/hrs/nrws
  
  # calculate time elapsed
  toc = round(Sys.time()-tic, 2)
  message('Done! Time elapsed: ', format(toc))
  message('Expected call rate: ', cr_mn_hr)
  message('Observed call rate: ', mean(cr_obs, digits = 2))
  
  return(df)
}

make_track = function(waypoints = 'data/raw/waypoints.csv',
                      spd = 0.1, # platform speed (m/s)
                      res = 60  # track time resolution (s)
){
  # read in waypoint list
  wpts = read_csv(waypoints, col_types = cols())
  
  # calculate distances between each point along path (meters)
  wpts$dist = 0
  for(ii in 2:nrow(wpts)){
    wpts$dist[ii] = sqrt((wpts$x[ii]-wpts$x[ii-1])^2 + (wpts$y[ii]-wpts$y[ii-1])^2)
  }
  
  # convert to cumulative distance traveled (meters)
  wpts$cdist = cumsum(wpts$dist)
  
  # calculate time to distance (seconds)
  wpts$time = wpts$cdist/spd

  # create time sequence (seconds)
  tseq = seq(from = res, to = ceiling(max(wpts$time)), by = res)
  
  # create an empty trackline grid
  tmp = tibble(x=NA, y=NA, time = tseq)
  
  # join, remove extra columns, and arrange in time
  trk = full_join(tmp,wpts,by=c("x","y","time")) %>%
    transmute(x, y, time) %>%
    arrange(time)
  
  # interpolate (finds x and y positions for the times in between the waypoints)
  trk$x = na.approx(trk$x)
  trk$y = na.approx(trk$y)
  
  # remove waypoint times (keeps sampling even)
  trk = trk[trk$time %in% c(0,tseq),]
  
  # print diagnostics
  message('Total number of waypoints: ', nrow(wpts))
  message('Total path distance: ', max(wpts$dist)/1e3, ' km')
  message('Total transit time: ', round(max(wpts$time)/60/60, 2), ' hr')
  
  return(trk)
}

detection_function = function(x,L=1.045,x0=10,k=-0.3){
  # Construct a detection function using a logistic curve
  # L = maximum Y value
  # x0 = value at midpoint
  # k = logistic growth rate
  y = L/(1+exp(-1*k*(x-x0))) 
  
  return(y)
}

simulate_detections = function(whale_df = wh, # whale movement model
                               track_df = trk # glider track
){
  #rename whale movement model table and lose unwanted variables 
  if("id" %in% colnames(whale_df)){
    colnames(whale_df) = c('id', 'x_wh', 'y_wh', 'time', 'ang', 'spd', 'dst', 'dpt', 'r', 'bh', 'call')
    whale_df = whale_df %>% transmute(id, x_wh, y_wh, time, call)
  } else {
    colnames(whale_df) = c('x_wh', 'y_wh', 'time', 'ang', 'spd', 'dst', 'dpt', 'r', 'bh', 'call')
    whale_df = whale_df %>% transmute(x_wh, y_wh, time, call) 
  } 
  
  #rename track movement model table 
  colnames(track_df) = c('x_dt', 'y_dt', 'time')
  
  # make data frame using whale movement variables
  df = merge(whale_df, track_df, by='time', all.x=TRUE)
  df$r_wh = sqrt((df$x_wh-df$x_dt)^2 + (df$y_wh-df$y_dt)^2)
  
  # subset to only times with calls
  calls = df %>% filter(call==1)
  
  # apply detection function to the call positions to extract probabilities of detection
  calls$p = detection_function(x = calls$r_wh)
  
  # generate a binomial distribution to see if each call was detected using this probability
  calls$detected = as.character(rbinom(n = nrow(calls), size = 1, prob = calls$p))

  # count and remove NAs
  unavailable = calls$p[complete.cases(calls)==FALSE]
  calls = calls[complete.cases(calls),]
  
  # find total number of detected calls
  detections = calls %>% filter(detected==1)
  
  # print diagnostics
  message('Number of calls unavailable to platform: ', length(unavailable))
  message('Number of calls available: ', nrow(calls))
  message('Number of calls detected: ', nrow(detections))
  message('Percent detection efficiency: ', 100*round((nrow(detections))/(nrow(calls)), 2), '%')

  return(calls)
}
