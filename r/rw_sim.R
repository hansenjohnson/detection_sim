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
  hrs = 24,       # number of hours
  dt = 2.5,       # time resolution [sec]
  x0 = 0,         # initial x position
  y0 = 0,         # initial y position
  bh = 'feeding', # behaviour (feeding, traveling, socializing)
  nt = 60,        # new time resolution after subsampling [sec]
  sub = TRUE,     # subsample data to new rate, nt
  cr_mn_hr = 0.25,# mean call rate (calls/whale/hr)
  cr_sd_hr = 0.025,# standard deviation of call rate,
  dtime_mean = 720,# mean dive time (s)
  dtime_sd = 180, # standard dev dive time (s)
  stime_mean = 300,# mean surface time (s)
  stime_sd = 60   # standard dev surface time (s)
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
  
  # add surfacing cycles from normal distribution
  # estimate cycle duration
  cycle_dur = dtime_mean+stime_mean
  
  # number of dive cycles to simulate (maximum estimate)
  n_cycles = ceiling(max(df$time)/cycle_dur)*2
  
  # generate distributions of surfacing and dive times
  stimes = rnorm(n = n_cycles, mean = stime_mean, sd = stime_sd)
  dtimes = rnorm(n = n_cycles, mean = dtime_mean, sd = dtime_sd)
  
  # generate table with alternating diving/surfacing and associated metadata
  cyc = tibble(
    # add dive and surfacing times to create one dive duration
    dive_dur = c(0, round(c(rbind(dtimes,stimes)),0)),
    # cummulative sum of all dives
    dive_time = cumsum(dive_dur),
    # dive number
    dive_index = seq(from=1, to = length(dive_time), by = 1),
    # alternate 0 and 1 to know when the whale is at the surface
    surface = as.character(rep(c(0,1), length.out = length(dive_time))) 
  )
  
  # bin whale movement wh by dive time
  df$dive_index = cut(x = df$time, breaks = cyc$dive_time, labels = F, include.lowest = TRUE)
  
  # merge dfs to include dive cycle info in movement df
  df = left_join(x = df, y = cyc, by = 'dive_index')
    
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
      rw_sim(x0=ini$x[i],y0=ini$y[i],hrs=hrs,bh=bh,nt=nt,cr_mn_hr=cr_mn_hr)
    })
    
  }
  
  # flatten list to combine all whales
  df = bind_rows(DF, .id = 'id')
  
  # convert time to hours
  # df$time = df$time/60/60
  
  # convert distance to kilometers
  df$x = df$x/1e3
  df$y = df$y/1e3
  df$r = df$r/1e3
  df$dst = df$dst/1e3
  df$dpt = df$dpt/1e3
  
  # calculate time elapsed
  toc = round(Sys.time()-tic, 2)
  message('Done! Time elapsed: ', format(toc))
  
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
  message('Total path distance: ', round(max(wpts$dist)/1e3), 2, ' km')
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
                               track_df = trk,# glider track
                               det_method = 'acoustic' # method of detection (calls versus surfacing)
){
  
  # deal with missing id column for single whale
  if(!"id" %in% colnames(whale_df)){
    whale_df$id = 1
  }
  
  #rename whale movement model table and lose unwanted variables
  whale_df = whale_df %>% transmute(id, x_wh=x, y_wh=y, time, call, dive_index, surface)
  
  #rename track movement model table 
  track_df = track_df %>% transmute(x_dt=x, y_dt=y, time)
  
  # make data frame using whale movement variables
  df = merge(whale_df, track_df, by='time', all.x=TRUE)
  df$r_wh = sqrt((df$x_wh-df$x_dt)^2 + (df$y_wh-df$y_dt)^2)

  if(det_method == 'acoustic'){
    # subset to only times with calls
    detections = df %>% filter(call==1) %>% select(-x_dt, -y_dt, -dive_index, -surface)
    # apply detection function to the call positions to extract probabilities of detection
    detections$p = detection_function(x = detections$r_wh, L = 1.045, x0 = 10, k = -0.3)
  } else if(det_method == 'visual'){
    # subset to only times with whale at the surface
    detections = df %>% filter(surface==1) %>% select(-x_dt, -y_dt, -call)
    # apply detection function to the surfacing positions to extract probabilities of detection
    detections$p = detection_function(x = detections$r_wh, L = 1, x0 = 1, k = -4.8)
  }
  
  # generate a binomial distribution to see if each call/surfacing was detected using this probability
  detections$detected = as.character(rbinom(n = nrow(detections), size = 1, prob = detections$p))
  
  # remove NAs
  detections = detections[complete.cases(detections),]
  
  return(detections)
}
  
