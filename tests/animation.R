## animation ##
# run model and animate tracks

# setup -------------------------------------------------------------------

library(tidyverse)
library(gifski)
library(gganimate)
source('r/rw_sim.R')

# process -----------------------------------------------------------------

# run model for multiple right whales
# model parameters
res = 60 # time res (s)

# run models
set.seed(1)
wh = rw_sim(hrs = 24, nt = res, sub = TRUE, bh = 'feeding') %>%
  mutate(
    x=x/1000,
    y=y/1000,
    r=r/1000
  )

set.seed(1)
whs = rw_sims(nrws = 10, hrs = 24, nt = res, bh = 'feeding', radius=20)

# modify time to hours for animation
wh$time = wh$time/60/60
whs$time = whs$time/60/60

# # coordinates of detector
# x_dt = 20
# y_dt = -40

# # make data frame using whale movement variables
# df = tibble(
#   id = whs$id,
#   t = whs$t,
#   x_whs = whs$x,
#   y_whs = whs$y,
#   x_dt,
#   y_dt,
#   r_whs = sqrt((x_whs-x_dt)^2 + (y_whs-y_dt)^2),
#   call = whs$call
# )
# 
# # subset to only times with calls
# calls = df %>% filter(call==1)
# 
# # apply detection function to the call positions to extract probabilities of detection
# calls$p = detection_function(x = calls$r_whs)
# 
# # generate a binomial distribution to see if each call was detected using this probability
# calls$detected = as.character(rbinom(n = nrow(calls), size = 1, prob = calls$p))

# create new grouping variables for animations
wh$grp = seq(1,nrow(wh))
whs$grp = seq(1,nrow(whs))

# recode values for plotting
whs$surface[whs$surface == 1] = 'surface'
whs$surface[whs$surface == 0] = 'dive'
whs$call[whs$call == 1] = 'call'

# plot tracks
p = ggplot()+
  geom_path(data=whs, aes(x=x,y=y, group=id,color=surface))+
  scale_color_manual(values = c('dive'='grey', 'surface'='black'))+
  geom_point(data=filter(whs,call=='call'), aes(x=x,y=y,group=grp,fill=call), shape = 21)+
  scale_fill_manual(values = 'black')+
  coord_equal()+
  labs(x = 'Easting (km)', y = 'Northing (km)', fill = NULL, color = NULL)+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = 'bottom')
p

# recode values for plotting
wh$surface[wh$surface == 1] = 'surface'
wh$surface[wh$surface == 0] = 'dive'
wh$call[wh$call == 1] = 'call'

p2 = ggplot()+
  geom_path(data=wh, aes(x=x,y=y, group=dive_index,color=surface))+
  scale_color_manual(values = c('dive'='grey', 'surface'='black'))+
  geom_point(data=filter(wh,call=='call'), aes(x=x,y=y,group=grp,fill=call), shape = 1, size=2)+
  scale_fill_manual(values = 'black')+
  coord_equal()+
  labs(x = 'Easting (km)', y = 'Northing (km)', fill = NULL, color = NULL)+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.text=element_text(size=12), legend.position = 'bottom')
p2
ggsave('figures/one_whale_figure.png', p2, height = 7, width = 6, units = 'in', dpi = 300)

# create animation
anim = p + 
  transition_reveal(along = time)+
  ggtitle("Hours: {round(frame_along,0)}")

anim2 = p2 + 
  transition_reveal(along = time)+
  ease_aes('linear')+
  ggtitle("Hours: {round(frame_along,0)}")

# render animation and save (might take some time)
anim
anim_save('figures/multiple_whales_animation2.gif', animation=anim)

anim2
anim_save('figures/one_whale_animation2.gif', animation=anim2)
