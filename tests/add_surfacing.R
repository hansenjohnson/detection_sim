## add_surfacing ##
# test to add surfacing behaviour to movement model

# input -------------------------------------------------------------------

# time resolution
res = 60

# mean dive time (s)
dtime_mean = 720

# standard dev dive time (s)
dtime_sd = 180

# mean surface time (s)
stime_mean = 300

# standard dev surface time (s)
stime_sd = 60

# setup -------------------------------------------------------------------

source('r/rw_sim.R')

# process -----------------------------------------------------------------

# run right whale model
wh = rw_sim(nt = res, sub = TRUE)%>%
  mutate(
    x=x/1000,
    y=y/1000,
    r=r/1000
  )

# estimate cycle duration
cycle_dur = dtime_mean+stime_mean

# number of dive cycles to simulate (maximum estimate)
n_cycles = ceiling(max(wh$time)/cycle_dur)*2

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
wh$dive_index = cut(x = wh$time, breaks = cyc$dive_time, labels = F, include.lowest = TRUE)

# merge dfs to include dive cycle info in movement df
wh = left_join(x = wh, y = cyc, by = 'dive_index')

# plot to check
ggplot()+
  geom_path(data = wh, aes(x=x,y=y,group=dive_index,color=surface))+
  scale_color_manual(values = c('0'='grey', '1'='black'))+
  geom_point(data=filter(wh,call==1), aes(x=x,y=y), shape = 21, fill = 'red')+
  coord_equal()+
  theme_bw()

# logistic curve example --------------------------------------------------

# input parameters
L=1
x0=1
k=-4.8
x=seq(from = 0, to = 4, by = 0.0005)

# calculate y
y = L/(1+exp(-1*k*(x-x0)))

# put in table
df = tibble(x=x,y=y)

# plot
ggplot()+
  geom_point(data=df,aes(x=x,y=y))

# apply detection function on one whale -------------------------------------

# coordinates of detector (in km)
x_dt = 0
y_dt = 0

# make data frame using whale movement variables
df = tibble(
  t = wh$time,
  x_wh = wh$x,
  y_wh = wh$y,
  x_dt,
  y_dt,
  r_wh = sqrt((x_wh-x_dt)^2 + (y_wh-y_dt)^2),
  call = wh$call,
  dive_index = wh$dive_index,
  surface = wh$surface
)

# subset to only times where the whale is at the surface
surfacing = df %>% filter(surface==1)

# apply detection function to the call positions to extract probabilities of detection
surfacing$p = detection_function(x = surfacing$r_wh, L=1.0, x0=1, k=-4.8)

# generate a binomial distribution to see if each call was detected using this probability
surfacing$detected = as.character(rbinom(n = nrow(surfacing), size = 1, prob = surfacing$p))

# plot to check
ggplot()+
  # plot whale track
  geom_path(data = wh, aes(x=x,y=y,group=dive_index,color=surface))+
  scale_color_manual(values = c('0'='grey', '1'='black'))+
  # plot detected surfacings
  geom_point(data = surfacing,aes(x=x_wh,y=y_wh,group=dive_index,fill=detected,size=detected),shape=21,alpha=0.7)+
  scale_size_manual(values = c('1'=2,'0'=1))+
  # plot detector position
  geom_point(aes(x=x_dt,y=y_dt),shape=24,fill='blue')+
  # formatting
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())

# plane detecting one whale surfacing -------------------------------------

# produce glider track and convert to km
track_df = make_track(res = res)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )

#rename whale movement model table and lose unwanted variables 
colnames(wh) = c('x_wh', 'y_wh', 'time', 'ang', 'spd', 'dst', 'dpt', 'r', 'bh', 'call', 'dive_index', 'dive_dur', 'dive_time', 'surface')
wh = wh %>% transmute(x_wh, y_wh, time, call, dive_index, surface)

#rename track movement model table 
colnames(track_df) = c('x_dt', 'y_dt', 'time')

# make data frame using whale movement variables
df = merge(wh, track_df, by='time', all.x=TRUE)
df$r_wh = sqrt((df$x_wh-df$x_dt)^2 + (df$y_wh-df$y_dt)^2)

# subset to only times with calls
surfacing = df %>% filter(surface==1) %>% transmute(x_wh, y_wh, surface, r_wh)

# apply detection function to the call positions to extract probabilities of detection
surfacing$p = detection_function(x = surfacing$r_wh, L=1.0, x0=1, k=-4.8)

# generate a binomial distribution to see if each call was detected using this probability
surfacing$detected = as.character(rbinom(n = nrow(surfacing), size = 1, prob = surfacing$p))

# plot to check
# plot both tracks (no detections)
ggplot()+
  geom_path(data = track_df, aes(x=x_dt,y=y_dt), color = 'blue')+
  geom_path(data = wh, aes(x=x_wh,y=y_wh), color = 'black')+
  geom_point(shape=1)+
  coord_equal()+
  theme_bw()

# plot both tracks with surfacing detected
ggplot()+
  # plot plane track
  geom_path(data = track_df,aes(x=x_dt,y=y_dt), color = 'black')+
  # plot whale track
  geom_path(data = wh,aes(x=x_wh,y=y_wh), color = 'grey')+
  # plot surfacing detections
  geom_point(data = surfacing,aes(x=x_wh,y=y_wh,fill=detected,size=detected),shape=21,alpha=0.7)+
  scale_fill_manual(values = c('1'='red','0'='grey'))+
  scale_size_manual(values = c('1'=2,'0'=1))+
  # formatting
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())

# plane detecting multiple whales surfacing -------------------------------------

whales_df = rw_sims(nrws = 2, hrs=24, nt = res, bh = 'feeding', cr_mn_hr = 2, radius = 10)

# estimate cycle duration
cycle_dur = dtime_mean+stime_mean

# number of dive cycles to simulate (maximum estimate)
n_cycles = ceiling(max(whales_df$time)/cycle_dur)*2

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
whales_df$dive_index = cut(x = whales_df$time, breaks = cyc$dive_time, labels = F, include.lowest = TRUE)

# merge dfs to include dive cycle info in movement df
whales_df = left_join(x = whales_df, y = cyc, by = 'dive_index')

# plot to check
ggplot()+
  geom_path(data = whales_df, aes(x=x,y=y,group=id,color=surface))+
  scale_color_manual(values = c('0'='grey', '1'='black'))+
  geom_point(data=filter(whales_df,call==1), aes(x=x,y=y), shape = 21, fill = 'red')+
  coord_equal()+
  theme_bw()

#rename whale movement model table and lose unwanted variables 
colnames(whales_df) = c('id', 'x_wh', 'y_wh', 'time', 'ang', 'spd', 'dst', 'dpt', 'r', 'bh', 'call', 'dive_index', 'dive_dur', 'dive_time', 'surface')
whales_df = whales_df %>% transmute(id, x_wh, y_wh, time, call, dive_index, surface)

#rename track movement model table 
colnames(track_df) = c('x_dt', 'y_dt', 'time')

# make data frame using whale movement variables
df = merge(whales_df, track_df, by='time', all.x=TRUE)
df$r_wh = sqrt((df$x_wh-df$x_dt)^2 + (df$y_wh-df$y_dt)^2)

# subset to only times with calls
surfacing = df %>% filter(surface==1) %>% transmute(id, x_wh, y_wh, surface, r_wh)

# apply detection function to the call positions to extract probabilities of detection
surfacing$p = detection_function(x = surfacing$r_wh, L=1.0, x0=1, k=-4.8)

# generate a binomial distribution to see if each call was detected using this probability
surfacing$detected = as.character(rbinom(n = nrow(surfacing), size = 1, prob = surfacing$p))

# plot to check
# plot both tracks (no detections)
ggplot()+
  geom_path(data = track_df, aes(x=x_dt,y=y_dt), color = 'blue')+
  geom_path(data = whales_df, aes(x=x_wh,y=y_wh, group=id), color = 'black')+
  geom_point(shape=1)+
  coord_equal()+
  theme_bw()

# plot both tracks with surfacing detected
ggplot()+
  # plot plane track
  geom_path(data = track_df,aes(x=x_dt,y=y_dt), color = 'black')+
  # plot whale track
  geom_path(data = whales_df,aes(x=x_wh,y=y_wh, group=id), color = 'grey')+
  # plot surfacing detections
  geom_point(data = surfacing,aes(x=x_wh,y=y_wh,fill=detected,size=detected),shape=21,alpha=0.7)+
  scale_fill_manual(values = c('1'='red','0'='grey'))+
  scale_size_manual(values = c('1'=2,'0'=1))+
  # formatting
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())
