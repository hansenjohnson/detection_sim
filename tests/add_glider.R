## add_glider ##
# functions for glider movement simulation

# setup -------------------------------------------------------------------

set.seed(1)

# libraries
library(tidyverse)
library(parallel)

source('tests/add_detector.R')

source('r/rw_sim.R')

# functions ---------------------------------------------------------------

glider_moving = function(
  # simulate glider movement with vectorized correlated random walk
  hrs = 24,       # number of hours
  dt = 2.5,       # time resolution [sec]
  x0 = 0,         # initial x position
  y0 = 0,         # initial y position
  bh = 'path1',   # glider path (path1, random, linear), can add more specific ones as we go
  nt = 60,        # new time resolution after subsampling [sec]
  sub = TRUE      # subsample data to new rate, nt
  )
{ 
  
  # define turn rate (in deg/10m)
  if(bh == 'path1'){
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

  return(df)
}

# test it ----------------------------------------------------------------

# run the model for a glider performing path1 over 24 hr period
df = glider_moving(hrs = hrs, bh = 'path1', sub = TRUE)

# plot glider track
ggplot(df, aes(x=x,y=y))+
  geom_path()+
  coord_equal()

# plot whale track
ggplot()+
  geom_path(data = wh,aes(x=x,y=y))+
  geom_point(data = filter(wh,call==1),aes(x=x,y=y),shape=21,fill='red')+
  coord_fixed()

# add detection capabilities -------------------------------------------------

# make data frame using whale movement variables
df = tibble(
  t = wh$t,
  x_wh = wh$x,
  y_wh = wh$y,
  x_dt = df$x,
  y_dt = df$y,
  r_wh = sqrt((x_wh-x_dt)^2 + (y_wh-y_dt)^2),
  call = wh$call
)

# subset to only times with calls
calls = df %>% filter(call==1)

# apply detection function to the call positions to extract probabilities of detection
calls$p = detection_function(x = calls$r_wh)

# generate a binomial distribution to see if each call was detected using this probability
calls$detected = as.character(rbinom(n = nrow(calls), size = 1, prob = calls$p))

# plot to check
plot = ggplot()+
  # plot glider track
  geom_path(data = df,aes(x=x_dt,y=y_dt), color = 'black')+
  # plot whale track
  geom_path(data = df,aes(x=x_wh,y=y_wh), color = 'grey')+
  # plot calls
  geom_point(data = calls,aes(x=x_wh,y=y_wh,fill=detected,size=detected),shape=21,alpha=0.7)+
  scale_fill_manual(values = c('1'='red','0'='grey'))+
  scale_size_manual(values = c('1'=2,'0'=1))+
  # formatting
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())
