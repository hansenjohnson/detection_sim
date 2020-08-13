## plot_whale_facet ##
# quick example of a time faceted whale track

# setup -------------------------------------------------------------------

source('r/rw_sim.R')

# one whale ---------------------------------------------------------------

# run rw_sim and convert to km
set.seed(1)
wh=rw_sim(hrs=24*7)%>%
  mutate(
    x=x/1000,
    y=y/1000,
    r=r/1000
  )

# convert time from s to hr
wh$hr = wh$time/60/60

# generate time bin
tbin = seq(from = 0, to = max(wh$hr), by = 24)

# use cut to assign each row to a given time bin
wh$tbin = cut(x = wh$hr, breaks = tbin, include.lowest = TRUE)

# regular plot
ggplot()+
  geom_path(data = wh,aes(x=x,y=y))+
  geom_point(data = filter(wh,call==1),aes(x=x,y=y),shape=21,fill='red')+
  coord_fixed()

# faceted plot 
ggplot()+
  geom_path(data = wh, aes(x=x,y=y))+
  geom_point(data = filter(wh, call==1), aes(x=x,y=y), color = 'red')+
  facet_wrap(~tbin)+
  coord_equal()+
  theme_bw()

# add detector (mooring)
# coordinates of detector
x_dt = -20
y_dt = -5

# make data frame using whale movement variables
df = tibble(
  t = wh$time,
  tbin = wh$tbin,
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
  geom_path(data = df,aes(x=x_wh,y=y_wh), color = 'grey')+
  # plot calls
  geom_point(data = calls,aes(x=x_wh,y=y_wh,fill=detected,size=detected),shape=21,alpha=0.7)+
  scale_fill_manual(values = c('1'='red','0'='black'))+
  scale_size_manual(values = c('1'=2,'0'=1))+
  # plot detector position
  geom_point(data = df, aes(x=x_dt,y=y_dt),shape=24,fill='blue')+
  # formatting
  facet_wrap(~tbin)+
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(x = 'x (km)', y = 'y (km)')

# multiple whales -------------------------------------------------------------

# run rw_sim, but no need convert to km
set.seed(1)
whs=rw_sims(nrws = 50, hrs= 24)

# no need to convert time from s to hr, already included in code

# generate time bin
tbin = seq(from = 0, to = max(whs$time), by = 6)

# use cut to assign each row to a given time bin
whs$tbin = cut(x = whs$time, breaks = tbin, include.lowest = TRUE)

# regular plot
ggplot()+
  geom_path(data=whs, aes(x=x,y=y, group=id))+
  geom_point(data=filter(whs,call==1),aes(x=x,y=y),shape=21, fill='red')+
  geom_path(alpha = 0.5)+
  coord_equal()

# faceted plot 
ggplot()+
  geom_path(data = whs, aes(x=x,y=y, group = id))+
  geom_point(data = filter(whs, call==1), aes(x=x,y=y), color = 'red')+
  facet_wrap(~tbin)+
  coord_equal()+
  theme_bw()
