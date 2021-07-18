## box_survey_functions ##
# help functions for box survey analysis

# setup -------------------------------------------------------------------

# libraries
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(zoo))
# suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(rgeos))
suppressPackageStartupMessages(library(raster))

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
  ini = init_whales(nrws = nrws, xmin=xmin*1e3, xmax=xmax*1e3, ymin=ymin*1e3, ymax=ymax*1e3)
  
  if(run_parallel){
    # determine number of cores available to run function more efficiently
    numCores = detectCores()
    
    if(!quiet){
      message('Running in parallel with ', numCores, ' cores...')
    }
    
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
  
  # assign speed (m/s) based on platform
  if(platform == 'slocum'){
    spd = 0.1 
  } else if (platform == 'plane'){
    spd = 51.4 
  } else if (platform == 'vessel'){
    spd = 4.1 
  } else if (platform == 'rpas'){
    spd = 41.2 
  } else {
    stop('Platform not recognized!')
  }
  
  # convert speed to km/s
  spd = spd/1e3
  
  # fixed params
  # move from left to right
  yres = 0.1
  ys = seq(from = ymin, to = ymax, by = yres)
  
  # define start and end points
  wpts = tibble(
    x=c(xmax,xmin), 
    y=sample(ys, size=2, replace = F),
    dist = 0,
    time = 0
  )
  
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

simulate_detections = function(whale_df = wh, track_df = trk, platform = 'slocum'){
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
  
  if(platform == 'slocum'){
    # subset to only times with calls
    detections = df %>% filter(call==1) %>% dplyr::select(-x_dt, -y_dt, -dive_index, -surface)
    # apply detection function to the call positions to extract probabilities of detection
    detections$p = detection_function(x = detections$r_wh, L = 1.045, x0 = 10, k = -0.3)
  } 
  else if(platform == 'plane'){
    # subset to only times with whale at the surface
    detections = df %>% filter(surface==1) %>% dplyr::select(-x_dt, -y_dt, -call)
    # apply detection function to the surfacing positions to extract probabilities of detection
    detections$p = detection_function(x = detections$r_wh, L = 1, x0 = 1, k = -4.8)
  } else if(platform == 'vessel'){
    # subset to only times with whale at the surface
    detections = df %>% filter(surface==1) %>% dplyr::select(-x_dt, -y_dt, -call)
    # apply detection function to the surfacing positions to extract probabilities of detection
    detections$p = detection_function(x = detections$r_wh, L = 1, x0 = 1, k = -4.8)
  } else if(platform == 'rpas'){
    # subset to only times with whale at the surface
    detections = df %>% filter(surface==1) %>% dplyr::select(-x_dt, -y_dt, -call)
    # assume all whales at surface within the field of view are detected
    detections$p = 0
    detections$p[detections$r_wh <= 0.175] = 1
  } else {
    stop('Unknown platform! Please choose: slocum, wave, plane, vessel, or rpas')
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
  
  rws %>%
    group_by(id) %>%
    summarize(
      reflect_rw(across(), ymax, ymin, xmax, xmin, verbose), .groups = 'drop'
    )
}

calculate_buffer = function(trk, 
                            platform='slocum', 
                            xmin=0, 
                            xmax=12, 
                            ymin=0, 
                            ymax=18, 
                            plot_check = FALSE){
  
  # assign buffer based on platform (range at 50% detection probability, km)
  if(platform == 'slocum'){
    bdist = 10 
  } else if (platform == 'plane'){
    bdist = 1.5 
  } else if (platform == 'vessel'){
    bdist = 1.5
  } else if (platform == 'rpas'){
    bdist = 0.172 
  } else {
    stop('Platform not recognized!')
  }
  
  # make list of point coordinates from platform track line
  points_coords = data.frame(x=trk$x, y=trk$y)
  
  # make a spatial lines object (turn coordinates into a line)
  lines_sp = SpatialLines(list(Lines(Line(points_coords), ID=1)), 
                          proj4string = CRS("+proj=laea +lat_0=45 +lon_0=-65 +datum=WGS84 +units=km"))
  
  # buffer line
  lines_buffer_sp = gBuffer(lines_sp, width = bdist, byid = TRUE)
  lines_buffer_sp = gBuffer(lines_buffer_sp, width = 0, byid = TRUE) # avoid invalid geom error
  
  # check validity
  if(gIsValid(lines_buffer_sp)){
    
    # find extent of survey box and crop crop buffer to within box
    ext = extent(xmin, xmax, ymin, ymax)
    lines_buffer_sp_cropped = crop(x = lines_buffer_sp, y = ext)
    
    # extract area of buffer (km2)
    if(class(lines_buffer_sp_cropped)=="SpatialPolygons"){
      a = lines_buffer_sp_cropped@polygons[[1]]@area  
    } else {
      a = NA
      message('Could not calculate area for ', platform, ' survey! Setting area to NA...')
    }
    
  } else {
    a = NA
    message('Invalid buffer for ', platform, ' survey! Setting area to NA...')
  }
  
  if(plot_check){
    plot(ext)
    plot(lines_sp, border="red", add=TRUE)
    plot(lines_buffer_sp, border="red", lty="dashed", add=TRUE)
    plot(lines_buffer_sp_cropped, border="green", lwd = 3, add=TRUE)
  }
  
  return(a)
}

box_survey = function(height = 18,
                      width = 12,
                      platform = 'slocum',
                      nrws = 3,
                      res = 2.5,
                      bh = 'feeding',
                      include_data = FALSE,
                      run_parallel = TRUE) {
  
  # simulate a transits of a given platform of a box containing nrws 
  
  # center grid on zero
  xmin = 0-width/2
  xmax = 0+width/2
  ymin = 0-height/2
  ymax = 0+height/2
  
  # create survey track
  trk = simulate_track(platform=platform,res=res,ymax,ymin,xmax,xmin)
  max_time = max(trk$time,na.rm = T)
  nhrs = ceiling(max_time/60/60)
  
  # find area surveyed by survey track
  area = calculate_buffer(trk,platform,xmin, xmax, ymin, ymax,plot_check = F)
  
  # simulate whales and reflect
  rws = rw_sims(
    nrws = nrws,
    hrs = nhrs,
    bh = bh,
    dt = res,
    nt = res,
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    run_parallel = run_parallel 
  ) %>% 
    filter(time <= max_time) %>%
    reflect_rws(., ymax, ymin, xmax, xmin)
  
  # simulate detections
  det = simulate_detections(whale_df = rws, track_df = trk, platform = platform)
  
  # detections per surfacing
  if('dive_index' %in% colnames(det)){
    det = det %>%
      mutate(
        detected = as.numeric(detected)
      ) %>%
      group_by(id,dive_index) %>%
      summarize(
        surface = unique(surface),
        p = mean(p, na.rm = TRUE),
        time = mean(time, na.rm = TRUE),
        x_wh = mean(x_wh, na.rm = TRUE),
        y_wh = mean(y_wh, na.rm = TRUE),
        r_wh = mean(r_wh, na.rm = TRUE),
        detected = sum(detected, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      dplyr::select(id, x_wh, y_wh, time, dive_index, surface, r_wh, p, detected)
    
    # convert to binary (0,1) detection
    det$detected[det$detected>0]=1
    
    # convert back to character
    det$detected = as.character(det$detected)
    
  }
  
  # select only detections
  det_only = filter(det, detected == 1) 
  
  if(!include_data){
    # summarize results
    df = tibble(
      platform = platform,
      n_whales = nrws,
      behavior = bh,
      transit_time = max_time,
      transit_dist = sqrt((trk$x[nrow(trk)]-trk$x[1])^2 + (trk$y[nrow(trk)]-trk$y[1])^2),
      transit_area = area,
      n_available = nrow(det),
      n_detected = nrow(filter(det,detected==1)),
      detected = ifelse(n_detected>0,1,0)#,
      #time_first_det = time_first_det,
      #dist_first_det = dist_first_det,
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
    df = tibble(
      platform = platform,
      n_whales = nrws,
      behavior = bh,
      transit_time = nhrs,
      transit_dist = sqrt((trk$x[nrow(trk)]-trk$x[1])^2 + (trk$y[nrow(trk)]-trk$y[1])^2),
      transit_area = area,
      n_available = nrow(det),
      n_detected = nrow(filter(det,detected==1)),
      detected = ifelse(n_detected>0,1,0),
      #time_first_det = time_first_det,
      #dist_first_det = dist_first_det,
      track_df = list(trk), # stores track dataframe in this column
      whale_df = list(rws), # stores whale movement dataframe in this column
      det_df = list(det),
      plot = list(p)
    )
  }
  
  return(df)
}

box_surveys = function(platform = 'slocum',
                       height = 18,
                       width = 12,
                       nrws = 3,
                       n_surveys = 10,
                       bh = 'feeding',
                       whales_parallel = FALSE,
                       survey_parallel = TRUE,
                       include_data = FALSE) {
  # complete multiple transits of a given platform of a box containing nrws 
  if(whales_parallel & survey_parallel){
    stop('Cannot process both whale tracks and surveys in parallel. Please choose one or the other.')
  }
  
  # define transit sequence
  nseq = seq(from = 1, to = n_surveys, by = 1)
  
  if(survey_parallel){
    # determine number of cores available to run function more efficiently
    numCores = detectCores()
    
    # model transits
    DF = mclapply(X = nseq, FUN = function(i){
      box_survey(
        platform = platform,
        height = height,
        width = width,
        nrws = nrws,
        bh = bh,
        include_data = include_data,
        run_parallel = whales_parallel
      )
    }, mc.cores = numCores)
    
  } else {
    
    # model transits
    DF = lapply(X = nseq, FUN = function(i){
      box_survey(
        platform = platform,
        height = height,
        width = width,
        nrws = nrws,
        bh = bh,
        include_data = include_data,
        run_parallel = whales_parallel
      )
    })
    
  }
  
  # flatten list to combine all surveys
  df = bind_rows(DF, .id = 'run')
  
  # print output stats
  return(df)
}

run_box_surveys = function(height = 18,
                           width = 12,
                           n_surveys = 10,
                           n_whales = c(1, 5, 10, 25, 50, 75),
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
    slo = box_surveys(
      platform = 'slocum',
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
    rpa = box_surveys(
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
    DF[[ii]] = bind_rows(slo, pln, ves, rpa)
    
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

# calculate time or distance to first detection
# this is included in the new output 
calc_first = function(x = time_first_det, y = transit_time){
  
  # pre-allocate variables
  td = rep(NA, length(which(!is.na(x))))
  cnt = 1
  t = 0
  
  # calculate transit x
  for(ii in 1:length(y)){
    if(is.na(x[ii])){
      t = t + y[ii]
    } else {
      td[cnt] = t + x[ii]
      t = 0
      cnt = cnt + 1
    }
  }
  
  return(mean(td, na.rm = TRUE))
}
