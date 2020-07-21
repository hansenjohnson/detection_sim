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

# logistic curve example --------------------------------------------------

# input parameters
L=1.0
x0=1500
k=-0.01
x=seq(from = 0, to = 4000, by = 0.5)

# calculate y
y = L/(1+exp(-1*k*(x-x0)))

# put in table
df = tibble(x=x,y=y)

# plot
ggplot()+
  geom_point(data=df,aes(x=x,y=y))

# simulate whale ----------------------------------------------------------

# model parameters
hrs = 24*7 # model run length (hr)
cr_hr = 3 # call rate (calls/hr)
nt = 60 # time res (s)
x0 = 3000 # start x coord (m)
y0 = 1000 # start y coord (m)

# run model and convert to km
wh = rw_sim(hrs = hrs, bh = 'feeding', sub = TRUE, nt = nt, x0=x0, y0=y0) %>%
  mutate(
    x=x/1000,
    y=y/1000,
    r=r/1000
  )

# calculate likelihood of call in timestep
cr_p = cr_hr/60/60*nt # KEY - this must be less than 1!

# generate a binomial distribution using this probability
wh$call = rbinom(n = nrow(wh), size = 1, prob = cr_p)

# plot to check
 ggplot()+
   geom_path(data = wh,aes(x=x,y=y))+
   geom_point(data = filter(wh,call==1),aes(x=x,y=y),shape=21,fill='red')+
   coord_fixed()

# test detection function -------------------------------------------------

# make detection function using the function created with these x-values
r = seq(from = 0, to = 40, by = 0.1)
p = detection_function(x = r)
df = tibble(r,p)

# quick plot to check
# ggplot(df,aes(x=r,y=p))+
#   geom_path()

# It works! Let's use the whale movements for x-values now

# apply detection function ------------------------------------------------

# coordinates of detector
x_dt = 20
y_dt = -40

# make data frame using whale movement variables
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

# apply detection function to the call positions to extract probabilities of detection
calls$p = detection_function(x = calls$r_wh)

# generate a binomial distribution to see if each call was detected using this probability
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
  
# find percentage of total calls detected
  detections = calls %>% filter(detected==1)
  x = nrow(detections)
  y = nrow(calls)
  (x/y)*100
