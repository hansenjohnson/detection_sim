## rw_sim ##
# functions for right whale movement simulation

# setup -------------------------------------------------------------------

# libraries
library(tidyverse)
library(parallel)

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
  t = seq(from = 0, to = hrs*60*60, by = dt)
  # create a sequence from 00 to 24 with 2.5 second jumps
  
  # length time vector
  n = length(t)
  
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
  df = tibble(x = x[1:n], y = y[1:n], t, ang = rad2deg(ang), spd, dst, dpt)
  
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
  dt = df$t[2] - df$t[1]
  
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
    message('Expected call rate: ', cr_mn_hr)
    message('Observed call rate: ', round(x = cr_obs, digits = 2))}
    else if(nrws>1){}
    
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
                   run_parallel = FALSE # run with parallel processing?
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
      rw_sim(x0=ini$x[i],y0=ini$y[i],hrs=hrs,bh=bh,nt=nt)
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
  df$t = df$t/60/60
  
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
