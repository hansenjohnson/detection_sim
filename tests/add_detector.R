## add_detector ##
# quick test to add a detector

# setup -------------------------------------------------------------------

set.seed(1)

library(tidyverse)

source('r/rw_sim.R')

detection_function = function(x,L=1.045,x0=10,k=-0.3){
  # Construct a detection function using a logistic curve
  # L = maximum Y value
  # x0 = value at midpoint
  # k = logistic growth rate
  y = L/(1+exp(-1*k*(x-x0))) 
  return(y)
}

# simulate whale ----------------------------------------------------------

# model parameters
hrs = 24*7 # model run length (hr)
cr_hr = 3 # call rate (calls/hr)
nt = 60 # time res (s)
x0 = 3000 # start x coord (m)
y0 = 1000 # start y coord (m)

# run model and convert to km
wh = rw_sim(hrs = hrs, bh = 'feeding', sub = TRUE, nt = nt,x0=x0,y0=y0) %>%
  mutate(
    x=x/1e3,
    y=y/1e3,
    r=r/1e3
  )

# calculate likelihood of call in timestep
cr_p = cr_hr/60/60*nt # KEY - this must be less than 1!

# generate a binomial distribution using this probability
wh$call = rbinom(n = nrow(wh), size = 1, prob = cr_p)

# plot to check
# ggplot()+
#   geom_path(data = wh,aes(x=x,y=y))+
#   geom_point(data = filter(wh,call==1),aes(x=x,y=y),shape=21,fill='red')

# test detection function -------------------------------------------------

# make detection function 
df = tibble(
  r = seq(from = 0, to = 40, by = 0.1), # range vector
  p = detection_function(x = r) # detection function
)

# quick plot to check
# ggplot(df,aes(x=r,y=p))+
#   geom_path()

# apply detection function ------------------------------------------------

# coordinates of detector
x_dt = 30
y_dt = -25

# make data frame
df = tibble(
  t = wh$t,
  x_wh = wh$x,
  y_wh = wh$y,
  x_dt,
  y_dt,
  r_wh = sqrt((x_wh-x_dt)^2 + (y_wh-y_dt)^2),
  call = wh$call
)

# subset to only times with calls
calls = df %>% filter(call==1)

# apply detection function to extract probabilities
calls$p = detection_function(x = calls$r_wh)

# apply binomial distribution to determine detection
calls$detected = as.character(rbinom(n = nrow(calls), size = 1, prob = calls$p))

# plot to check
ggplot()+
  # plot whale track
  geom_path(data = wh,aes(x=x,y=y), color = 'grey')+
  
  # plot calls
  geom_point(data = calls,aes(x=x_wh,y=y_wh,fill=detected,size=detected),shape=21,alpha=0.7)+
  scale_fill_manual(values = c('1'='red','0'='grey'))+
  scale_size_manual(values = c('1'=2,'0'=1))+
  
  # plot detector position
  geom_point(aes(x=x_dt,y=y_dt),shape=24,fill='blue')+
  
  # formatting
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())
  
  
