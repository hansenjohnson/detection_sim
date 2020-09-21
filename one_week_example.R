## one_week_example ##
# simulation of whales moving for one week with four platforms

# setup -------------------------------------------------------------------

source('r/rw_sim.R')

# create whales and tracks ------------------------------------------------

# define time resolution (s)
res = 60

# produce whale movement model
set.seed(1)
whales = rw_sims(nrws = 10, hrs=24*7, nt = res, bh = 'feeding')

# plot whale with calls and surfacing
ggplot()+
  geom_path(data = whales, aes(x=x,y=y,group=id,color=surface))+
  scale_color_manual(values = c('0'='grey', '1'='black'))+
  geom_point(data=filter(whales,call==1), aes(x=x,y=y), shape = 21, fill = 'red')+
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())

# produce first plane track and convert to km
plane_1 = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 51)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )

# produce second plane track and convert to km
plane_2 = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 50)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )

# start flights at 10am and 2pm, respectively 
# say time 0 is midnight on the first day the whales are moving
plane_1$time = plane_1$time + 10*60*60
plane_2$time = plane_2$time + 14*60*60

# produce other planes tracks and convert to km
plane_3 = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 51)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )
plane_4 = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 50)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )
plane_5 = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 51)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )
plane_6 = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 50)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )
plane_7 = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 51)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )
plane_8 = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 50)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )
plane_9 = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 51)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )
plane_10 = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 50)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )
plane_11 = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 51)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )
plane_12 = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 50)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )
plane_13 = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 51)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )
plane_14 = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 50)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )

# stagger flights so that there are two per day 
plane_3$time = plane_1$time + 24*60*60
plane_4$time = plane_2$time + 24*60*60
plane_5$time = plane_1$time + 48*60*60
plane_6$time = plane_2$time + 48*60*60
plane_7$time = plane_1$time + 72*60*60
plane_8$time = plane_2$time + 72*60*60
plane_9$time = plane_1$time + 96*60*60
plane_10$time = plane_2$time + 96*60*60
plane_11$time = plane_1$time + 120*60*60
plane_12$time = plane_2$time + 120*60*60
plane_13$time = plane_1$time + 144*60*60
plane_14$time = plane_2$time + 144*60*60

# produce glider track and convert to km
glider = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 0.1)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )
# select only glider track that occurs while the whale is available
glider = glider %>% filter(glider$time<=max(whales$time))

# produce entire vessel track and convert to km
vessel = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 4)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )

# generate time bin to separate vessel track into 10 hour bits
vessel$hr = vessel$time/60/60
tbin = seq(from = 0, to = max(vessel$hr), by = 10)
vessel$tbin = cut(x = vessel$hr, breaks = tbin, include.lowest = TRUE)

# make two deployments 
vessel_1 = vessel %>% filter(tbin=="[0,10]") %>% select(-tbin)
vessel_2 = vessel[-c(1:nrow(vessel_1)),-c(5)]

# start vessel tracks at at 8am on different days
# say time 0 is midnight on the first day the whales are moving
vessel_1$time = vessel_1$time + 8*60*60
vessel_2$time = vessel_2$time + 32*60*60

# repeat three more times
vessel_b = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 3.8)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )
vessel_b$hr = vessel_b$time/60/60
tbin = seq(from = 0, to = max(vessel_b$hr), by = 10)
vessel_b$tbin = cut(x = vessel_b$hr, breaks = tbin, include.lowest = TRUE)
vessel_3 = vessel_b %>% filter(tbin=="[0,10]") %>% select(-tbin)
vessel_4 = vessel_b[-c(1:nrow(vessel_3)),-c(5)]
vessel_3$time = vessel_3$time + 56*60*60
vessel_4$time = vessel_4$time + 80*60*60

vessel_c = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 3.9)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )
vessel_c$hr = vessel_c$time/60/60
tbin = seq(from = 0, to = max(vessel_c$hr), by = 10)
vessel_c$tbin = cut(x = vessel_c$hr, breaks = tbin, include.lowest = TRUE)
vessel_5 = vessel_c %>% filter(tbin=="[0,10]") %>% select(-tbin)
vessel_6 = vessel_c[-c(1:nrow(vessel_5)),-c(5)]
vessel_5$time = vessel_5$time + 104*60*60
vessel_6$time = vessel_6$time + 128*60*60

vessel_d = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 4.1)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )
vessel_d$hr = vessel_d$time/60/60
tbin = seq(from = 0, to = max(vessel_d$hr), by = 10)
vessel_d$tbin = cut(x = vessel_d$hr, breaks = tbin, include.lowest = TRUE)
vessel_7 = vessel_d %>% filter(tbin=="[0,10]") %>% select(-tbin)
vessel_7$time = vessel_7$time + 152*60*60

# add mooring
x_moor = 0
y_moor = 0

# make data frame using whale movement variables
df = tibble(
  time = whales$time,
  x_wh = whales$x,
  y_wh = whales$y,
  x_moor,
  y_moor,
  r_wh = sqrt((x_wh-x_moor)^2 + (y_wh-y_moor)^2),
  call = whales$call
)

# simulate detections ------------------------------------------------

# simulate detection capabilities of platforms
plane_det_1 = simulate_detections(whale_df = whales, track_df = plane_1, det_method = 'visual')
plane_det_2 = simulate_detections(whale_df = whales, track_df = plane_2, det_method = 'visual')
plane_det_3 = simulate_detections(whale_df = whales, track_df = plane_3, det_method = 'visual')
plane_det_4 = simulate_detections(whale_df = whales, track_df = plane_4, det_method = 'visual')
plane_det_5 = simulate_detections(whale_df = whales, track_df = plane_5, det_method = 'visual')
plane_det_6 = simulate_detections(whale_df = whales, track_df = plane_6, det_method = 'visual')
plane_det_7 = simulate_detections(whale_df = whales, track_df = plane_7, det_method = 'visual')
plane_det_8 = simulate_detections(whale_df = whales, track_df = plane_8, det_method = 'visual')
plane_det_9 = simulate_detections(whale_df = whales, track_df = plane_9, det_method = 'visual')
plane_det_10 = simulate_detections(whale_df = whales, track_df = plane_10, det_method = 'visual')
plane_det_11 = simulate_detections(whale_df = whales, track_df = plane_11, det_method = 'visual')
plane_det_12 = simulate_detections(whale_df = whales, track_df = plane_12, det_method = 'visual')
plane_det_13 = simulate_detections(whale_df = whales, track_df = plane_13, det_method = 'visual')
plane_det_14 = simulate_detections(whale_df = whales, track_df = plane_14, det_method = 'visual')

glider_det = simulate_detections(whale_df = whales, track_df = glider, det_method = 'acoustic')

vessel_det_1 = simulate_detections(whale_df = whales, track_df = vessel_1, det_method = 'visual')
vessel_det_2 = simulate_detections(whale_df = whales, track_df = vessel_2, det_method = 'visual')
vessel_det_3 = simulate_detections(whale_df = whales, track_df = vessel_3, det_method = 'visual')
vessel_det_4 = simulate_detections(whale_df = whales, track_df = vessel_4, det_method = 'visual')
vessel_det_5 = simulate_detections(whale_df = whales, track_df = vessel_5, det_method = 'visual')
vessel_det_6 = simulate_detections(whale_df = whales, track_df = vessel_6, det_method = 'visual')
vessel_det_7 = simulate_detections(whale_df = whales, track_df = vessel_7, det_method = 'visual')

# add column identifying flight number
plane_det_1$flight_id = '1'
plane_det_2$flight_id = '2'
plane_det_3$flight_id = '3'
plane_det_4$flight_id = '4'
plane_det_5$flight_id = '5'
plane_det_6$flight_id = '6'
plane_det_7$flight_id = '7'
plane_det_8$flight_id = '8'
plane_det_9$flight_id = '9'
plane_det_10$flight_id = '10'
plane_det_11$flight_id = '11'
plane_det_12$flight_id = '12'
plane_det_13$flight_id = '13'
plane_det_14$flight_id = '14'

# merge plane detections
visual_det = rbind(plane_det_1, plane_det_2, plane_det_3, plane_det_4, plane_det_5, plane_det_6, 
                   plane_det_7, plane_det_8, plane_det_9, plane_det_10, plane_det_11, plane_det_12,
                   plane_det_13, plane_det_14)

