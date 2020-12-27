## box_survey_functions ##
# help functions for box survey analysis

library(tidyverse)

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
  # causes the angular scale ‘circular’ (but in radians rather than degrees) 
  # so that if an angle is added to 365, it restarts the scale and doesn't >365
  
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
  # we multiply by 2 to ensure that we simulate enough dive cycles
  
  # generate distributions of surfacing and dive times
  stimes = rnorm(n = n_cycles, mean = stime_mean, sd = stime_sd)
  dtimes = rnorm(n = n_cycles, mean = dtime_mean, sd = dtime_sd)
  
  # determine if start in a dive or not
  start_in_dive = rbinom(1, 1, prob = (dtime_mean/(dtime_mean+stime_mean)))
  #gives an 71% chance that the whale starts in a dive (1 is YES or 0 NO)
  #if it is 1, YES, it starts in a dive, as is written in the if statement
  
  # generate table with alternating diving/surfacing and associated metadata
  if(start_in_dive){
    cyc = tibble(
      # add dive and surfacing times to create one dive duration
      dive_dur = round(c(rbind(dtimes,stimes))),
      # cummulative sum of all dives
      dive_time = cumsum(c(0,dive_dur[1:(length(dive_dur)-1)])),
      # dive number
      dive_index = seq(from=1, to = length(dive_time), by = 1),
      # alternate 0 and 1 to know when the whale is at the surface
      surface = as.character(rep(c(0,1), length.out = length(dive_time))) 
      #This line makes a vector that indicates the whale’s state, 
      #where 0 indicates the whale is in a dive and 1 indicates it is at the surface 
      #The order of the 0 and 1 depend on the starting state of the whale, 
      #hence why they change order for "else"
    )
  } else {
    cyc = tibble(
      # add dive and surfacing times to create one dive duration
      dive_dur = round(c(rbind(stimes,dtimes))),
      # cummulative sum of all dives
      dive_time = cumsum(c(0,dive_dur[1:(length(dive_dur)-1)])),
      # dive number
      dive_index = seq(from=1, to = length(dive_time), by = 1),
      # alternate 0 and 1 to know when the whale is at the surface
      surface = as.character(rep(c(1,0), length.out = length(dive_time))) 
    )
  }
  
  # bin whale movement wh by dive time
  df$dive_index = cut(x = df$time, breaks = unique(cyc$dive_time), labels = F, include.lowest = TRUE)
 
  # merge dfs to include dive cycle info in movement df
  df = left_join(x = df, y = cyc, by = 'dive_index')
  
  return(df)
}

init_whales = function(nrws=1e3, xmin, xmax, ymin, ymax){
  # initialize field of simulated whales within a box (km)
  
  # dropping the points (assigns an angle between 0-2pi based on a specific radius and transforms that into range)
  # calculate angles and ranges
  #a = 2*pi*runif(n = nrws, min = 0, max = 1)
  #r = radius*sqrt(runif(n = nrws, min = 0, max = 1))

  # convert to xy
  #x=r*cos(a)
  #y=r*sin(a)
  
  a = runif(n = nrws, min = xmin, max = xmax)
  b = runif(n = nrws, min = ymin, max = ymax)

  # return data
  out = data.frame(x=round(a),y=round(b))
}

rw_sims = function(nrws = 1e2,          # number of whales in simulation
                   hrs = 48,            # duration of simulation (hours)
                   bh = 'feeding',      # behaviour
                   dt = 2.5,            # movement model time resolution (seconds)
                   nt = 300,            # output time resolution (seconds)
                   xmin,                # xmin of initial positions (km)
                   xmax,                # xmax of initial positions (km)
                   ymin,                # ymin of initial positions (km)
                   ymax,                # ymax of initial positions (km)
                   run_parallel = TRUE,# run with parallel processing?
                   cr_mn_hr = 0.25,     # mean call rate (calls/whale/hr)
                   quiet = TRUE         # hide startup message?
){
  # simulate movements of a field of numerous whales
  
  # startup message
  if(!quiet){
    message('\n## NARW MOVEMENT MODEL RUN ##\n')
    message('Input parameters:')
    message('   number of whales: ', nrws)
    message('   number of hours: ', hrs)
    message('   time resolution [sec]: ', nt)
    message('   number of timesteps: ', hrs*60*60/nt)
    message('   movement type: ', bh)
    message('Simulating whale movements...')
  }
  
  # initialize data values
  nseq = seq(from = 1, to = nrws, by = 1)
  tic = Sys.time()
  
  # initial starting positions
  ini = init_whales(nrws = nrws, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
  
  if(run_parallel){
    # determine number of cores available to run function more efficiently
    numCores = detectCores()
    
    #message('Running in parallel with ', numCores, ' cores...')
    
    # model movements
    DF = mclapply(X = nseq, FUN = function(i){
      rw_sim(x0=ini$x[i],y0=ini$y[i],hrs=hrs,bh=bh,dt=dt,nt=nt,cr_mn_hr=cr_mn_hr)
    }, mc.cores = numCores)
    
  } else {
    
    # model movements
    DF = lapply(X = nseq, FUN = function(i){
      rw_sim(x0=ini$x[i],y0=ini$y[i],hrs=hrs,bh=bh,dt=dt,nt=nt,cr_mn_hr=cr_mn_hr)
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
  if(!quiet){
    message('Done! Time elapsed: ', format(toc))
  }
  
  return(df)
}

simulate_track = function(platform,res=2.5,ymax,ymin,xmax,xmin){
  # simulate a transit of a box by a given platform
  
  # assign speed based on platform
  if(platform == 'glider'){
    spd = 0.1 # platform speed (m/s)
  } else if (platform == 'plane'){
    spd = 51 # platform speed (m/s)
  } else if (platform == 'vessel'){
    spd = 4 
  } else {
    stop('Platform not recognized!')
  }
  
  # convert speed to km/s
  spd = spd/1e3
  
  # fixed params
  # move from left to right
  yres = 0.1
  ys = seq(from = ymin, to = ymax, by = yres)
  
  # move from top to bottom
  #xres = 0.1
  #xs = seq(from = xmin, to = xmax, by = xres)
  
  # define start and end points
  wpts = tibble(
    x=c(xmax,xmin), 
    y=sample(ys, size=2, replace = F),
    dist = 0,
    time = 0
  )
  
  #wpts = tibble(
    #x=sample(xs, size=2, replace = F), 
    #y=c(ymax,ymin),
    #dist = 0,
    #time = 0
  #)
  
  # calculate travel distance
  wpts$dist[2] = sqrt((wpts$x[2]-wpts$x[1])^2 + (wpts$y[2]-wpts$y[1])^2)
  
  # calculate transit time
  wpts$time = wpts$dist/spd
  
  # create time sequence (seconds)
  tseq = seq(from = res, to = max((wpts$time[2]) - res), by = res)
  
  # create an empty trackline grid
  tmp = tibble(x=NA, y=NA, time = tseq)
  
  # join, remove extra columns, and arrange in time
  trk = full_join(tmp,wpts,by=c("x","y","time")) %>%
    transmute(x, y, time) %>%
    arrange(time)
  
  # remove times after last waypoint is reached
  #trk = trk %>% filter(time<=max(wpts$time))
  # if this step is not done, trk length may be > than wpts and this creates errors in na.approx
  
  # interpolate (finds x and y positions for the times in between the waypoints)
  trk$x = na.approx(trk$x)
  trk$y = na.approx(trk$y)
  
  # remove waypoint times (keeps sampling even)
  trk = trk[trk$time %in% c(0,tseq),]
  
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

simulate_detections = function(whale_df = wh, track_df = trk,det_method = 'acoustic'){
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

reflect_rw = function(rw,ymax,ymin,xmax,xmin,verbose=FALSE){
  # contain a single right whale within a box
  
  f_all = f_ymax = f_ymin = f_xmax = f_xmin = 0
  while(f_all == 0){
  # for when you don't know how many for loops you need
    
    if(length(rw$y[rw$y>ymax])>0){
      rw$y[rw$y>ymax] = 2*ymax-rw$y[rw$y>ymax]  
      if(verbose){message('Reflecting about y max')}
    } else {
      f_ymax = 1
    }
    
    if(length(rw$y[rw$y<ymin])>0){
      rw$y[rw$y<ymin] = 2*ymin-rw$y[rw$y<ymin]  
      if(verbose){message('Reflecting about y min')}
    } else {
      f_ymin = 1
    }
    
    if(length(rw$x[rw$x>xmax])>0){
      rw$x[rw$x>xmax] = 2*xmax-rw$x[rw$x>xmax]  
      if(verbose){message('Reflecting about x max')}
    } else {
      f_xmax = 1
    }
    
    if(length(rw$x[rw$x<xmin])>0){
      rw$x[rw$x<xmin] = 2*xmin-rw$x[rw$x<xmin]  
      if(verbose){message('Reflecting about x min')}
    } else {
      f_xmin = 1
    }
    
    if(f_ymax == 1 & f_ymin == 1 & f_xmax == 1 & f_xmin == 1){
      f_all = 1
    # only when all the sides are satisfied, the while loop stops because f_all is not 0 anymore
    }
  }
  
  return(rw)
  
}

reflect_rws = function(rws,ymax,ymin,xmax,xmin,verbose=FALSE){
  # contain multiple right whales within a box

  # reflect
  rw2 = rws %>%
    group_by(id) %>%
    do(reflect_rw(.,ymax,ymin,xmax,xmin,verbose)) %>%
    ungroup()
  
  return(rw2)
}

run_box_survey = function(height=18,width=12,platform='glider',nrws = 3,n_surveys=10,res=2.5,bh='feeding',include_data=F){
  # complete a multiple transits of a given platform of a box containing nrws 
  
  # define radius
  r = min(c(height,width))/2
  
  # define centered on zero
  xmin = 0-width/2
  xmax = 0+width/2
  ymin = 0-height/2
  ymax = 0+height/2
  
  # allocate output
  DF = vector('list', length = n_surveys)
  for(ii in 1:n_surveys){
    
    # create survey track
    trk = simulate_track(platform=platform,res=res,ymax,ymin,xmax,xmin)
    max_time = max(trk$time,na.rm = T)
    nhrs = ceiling(max_time/60/60)
    
    # simulate whales and reflect
    rws = rw_sims(nrws = nrws, hrs = nhrs, bh=bh, dt = res, nt = res, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax) %>%
      filter(time <= max_time) %>%
      reflect_rws(.,ymax,ymin,xmax,xmin)
    
    # simulate detections
    det_method = ifelse(platform %in% c('glider','buoy'), 'acoustic','visual')
    det = simulate_detections(whale_df = rws, track_df = trk, det_method = det_method)
    
    if(!include_data){
      # summarize results
      DF[[ii]] = tibble(
        run = ii,
        platform = platform,
        n_whales = nrws,
        behavior = bh,
        transit_time = nhrs,
        transit_dist = sqrt((trk$x[nrow(trk)]-trk$x[1])^2 + (trk$y[nrow(trk)]-trk$y[1])^2),
        n_available = nrow(det),
        n_detected = nrow(filter(det,detected==1)),
        detected = ifelse(n_detected>0,1,0)
      )
    } else {
      # plot
      rws$sid = paste0(rws$id, '_',rws$dive_index)
      p = ggplot()+
        geom_rect(aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax),fill=NA,color='grey')+
        geom_path(data=trk,aes(x=x,y=y),color='blue')+
        geom_path(data=rws,aes(x=x,y=y,group=sid,color=surface))+
        geom_point(data=filter(rws,call==1),aes(x=x,y=y),shape=1)+
        geom_point(data=filter(det,detected==1), aes(x=x_wh,y=y_wh), shape = 21, fill = 'red')+
        scale_color_manual(values = c('grey','black'))+
        labs(x='Easting (km)',y='Northing (km)')+
        coord_equal()+
        theme_bw()+
        theme(panel.grid = element_blank(), legend.position = "none")
      
      # summarize results
      DF[[ii]] = tibble(
        run = ii,
        platform = platform,
        n_whales = nrws,
        behavior = bh,
        transit_time = nhrs,
        transit_dist = sqrt((trk$x[nrow(trk)]-trk$x[1])^2 + (trk$y[nrow(trk)]-trk$y[1])^2),
        n_available = nrow(det),
        n_detected = nrow(filter(det,detected==1)),
        detected = ifelse(n_detected>0,1,0),
        track_df = list(trk), # stores track dataframe in this column
        whale_df = list(rws), # stores whale movement dataframe in this column
        det_df = list(det),
        plot = list(p)
      )
    }
  }
  
  # combine all
  df = bind_rows(DF)
  
  return(df)
}

run_box_surveys = function(height=18,width=12,nrws = 3,n_surveys=10,bh='feeding'){
  # complete a single transit of a given platform of a box containing nrws 
  
  # run each platform
  pln = run_box_survey(platform='plane',height=height,width=width,nrws=nrws,n_surveys=n_surveys,bh=bh)
  ves = run_box_survey(platform='vessel',height=height,width=width,nrws=nrws,n_surveys=n_surveys,bh=bh)
  gld = run_box_survey(platform='glider',height=height,width=width,nrws=nrws,n_surveys=n_surveys,bh=bh)
  
  # combine
  df = rbind(pln,ves,gld)
  
  # compute summary stats
  out = df %>% group_by(platform) %>%
    summarize(
      n_whales = unique(n_whales),
      behavior = unique(behavior),
      transits = length(detected),
      transit_time = mean(transit_time),
      transit_dist = mean(transit_dist),
      transits_with_detections = sum(detected),
      transit_p = transits_with_detections/transits,
      .groups = 'drop')
  
  # print output stats
  return(out)
}

