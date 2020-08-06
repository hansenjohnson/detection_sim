## animation ##
# run model and animate tracks

# setup -------------------------------------------------------------------

install.packages('gifski')
install.packages('gganimate')
library(tidyverse)
library(gifski)
library(gganimate)
source('r/rw_sim.R')
source('r/detect_calls.R')

# process -----------------------------------------------------------------

# run model for multiple right whales
# model parameters
hrs = 24*7 # model run length (hr)
nt = 60 # time res (s)
nrws = 25

# run model
whs = rw_sims(nrws = nrws, hrs = hrs, bh = 'feeding', nt = nt)

# coordinates of detector
x_dt = 20
y_dt = -40

# make data frame using whale movement variables
df = tibble(
  id = whs$id,
  t = whs$t,
  x_whs = whs$x,
  y_whs = whs$y,
  x_dt,
  y_dt,
  r_whs = sqrt((x_whs-x_dt)^2 + (y_whs-y_dt)^2),
  call = whs$call
)

# subset to only times with calls
calls = df %>% filter(call==1)

# apply detection function to the call positions to extract probabilities of detection
calls$p = detection_function(x = calls$r_whs)

# generate a binomial distribution to see if each call was detected using this probability
calls$detected = as.character(rbinom(n = nrow(calls), size = 1, prob = calls$p))

# plot tracks
p = ggplot(data=df, aes(x=x_whs,y=y_whs, group=id))+
  geom_path(alpha = 0.5, color = 'grey')+
  geom_point(alpha = 0.7, shape=21, size = 2, fill = 'black')+
  geom_point(data = calls,aes(x=x_whs,y=y_whs,fill=detected,size=detected),shape=21,alpha=0.7)+
  coord_equal()+
  labs(x = 'Easting (km)', y = 'Northing (km)')+
  theme_bw()+
  theme(panel.grid = element_blank())
p

# create animation
anim = p + 
  transition_reveal(along = t)+
  ease_aes('linear')+
  ggtitle("Hours: {round(frame_along,0)}")

# render animation (might take some time)
anim
