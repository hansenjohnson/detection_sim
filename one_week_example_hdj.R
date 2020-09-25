## one_week_example ##
# simulation of whales moving for one week with 3 platforms

# setup -------------------------------------------------------------------

source('r/rw_sim.R')

# define time resolution (s)
res = 3

# make reproducible
set.seed(1)

# whales ------------------------------------------------------------------

whales = rw_sims(nrws = 25, hrs=24*7, nt = res, bh = 'feeding', radius = 25)

# plane -------------------------------------------------------------------

# define start times of flight 1 and 2 (in seconds since midnight)
f1_start = 10*60*60
f2_start = 14*60*60

# make single track
pl = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 51)%>%
  mutate(
    x=x/1000,
    y=y/1000,
    platform = 'plane'
  )

# define a list to hold finished tracks
PL = vector('list', 14) # list to hold finished tracks

# define a 'counter' variable to keep track of flight ids
flight_id=1

# loop through each day to add flight times/ids
for(iday in 1:7){
  
  # flight 1
  PL[[flight_id]] = pl %>%
    mutate(time = time+f1_start+24*(iday-1)*60*60,
           id = flight_id)
  
  # update flight id
  flight_id=flight_id+1
  
  # flight 2
  PL[[flight_id]] = pl %>%
    mutate(time = time+f2_start+24*(iday-1)*60*60,
           id = flight_id)
  
  # update flight id
  flight_id=flight_id+1
}

# combine all flights
plane_trk = bind_rows(PL)

# process detections
plane_det = simulate_detections(whale_df = whales, track_df = plane_trk, det_method = 'visual')

# vessel ------------------------------------------------------------------

# vessel start time (in seconds since midnight)
v_start = 8*60*60

# make a vessel track
vessel = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 4)%>%
  mutate(
    x=x/1000,
    y=y/1000,
    platform = 'vessel'
  )

# find midpoint of track
mp = round(max(vessel$time)/2)

# split it in half
v1 = vessel %>% filter(time<mp)
v2 = vessel %>% filter(time>=mp) %>% mutate(time=time-mp)

# define a list to hold finished tracks
VE = vector('list', 7) # list to hold finished tracks

# define which half of the vessel track to use each day
ves = rep(c(1,2),length=7)

# loop through and choose which track to use
for(ii in seq_along(ves)){
  if(ves[ii]==1){
    VE[[ii]] = v1 %>% mutate(time = time+v_start+24*(ii-1)*60*60, id = ii) 
  } else {
    VE[[ii]] = v2 %>% mutate(time = time+v_start+24*(ii-1)*60*60, id = ii) 
  }
}

# combine all flights
vessel_trk = bind_rows(VE)

# process detections
vessel_det = simulate_detections(whale_df = whales, track_df = vessel_trk, det_method = 'visual')

# glider ------------------------------------------------------------------

# make glider track and restrict to whale time
glider_trk = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 0.1)%>%
  filter(time<=max(whales$time)) %>%
  mutate(
    x=x/1000,
    y=y/1000,
    id = 1,
    platform = 'glider'
  )

# simulate detections
glider_det = simulate_detections(whale_df = whales, track_df = glider_trk, det_method = 'acoustic')

# mooring -----------------------------------------------------------------

# add mooring
x = 0
y = 0

# make data frame using whale movement variables
buoy_trk = tibble(
  x,
  y,
  time = glider_trk$time,
  id = 1,
  platform = 'buoy'
)

# simulate detections
buoy_det = simulate_detections(whale_df = whales, track_df = buoy_trk, det_method = 'acoustic')

# know how many calls and surfacings were detected
glider_det %>% group_by(detected) %>% count()
buoy_det %>% group_by(detected) %>% count()
vessel_det %>% group_by(detected) %>% count()
plane_det %>% group_by(detected) %>% count()

# format for plotting -----------------------------------------------------

glider_det = glider_det %>%
  select(-call) %>%
  mutate(platform = 'glider')

buoy_det = buoy_det %>%
  select(-call) %>%
  mutate(platform = 'buoy')

plane_det = plane_det %>%
  select(-surface,-dive_index) %>%
  mutate(platform = 'plane')

vessel_det = vessel_det %>%
  select(-surface,-dive_index) %>%
  mutate(platform = 'vessel')

# combine detections
det = rbind(glider_det,buoy_det,plane_det,vessel_det)

# combine tracks
trk = rbind(glider_trk, buoy_trk, plane_trk, vessel_trk)

# add day column
tbins = seq(from = 0, to = 24*7, by = 24)
det$day = cut(det$time/60/60, breaks = tbins, include.lowest = T)
trk$day = cut(trk$time/60/60, breaks = tbins, include.lowest = T)
whales$day = cut(whales$time/60/60, breaks = tbins, include.lowest = T)

# plot --------------------------------------------------------------------

# downsample data
whales = whales[seq(from = 1, to = nrow(whales), by = round(60/res)),]
trk = trk[seq(from = 1, to = nrow(trk), by = round(60/res)),]
det = det[seq(from = 1, to = nrow(det), by = round(60/res)),]

# plot
p = ggplot()+
  
  # whales
  geom_path(data = whales, aes(x=x,y=y,group=id),color='grey')+
  
  # tracks
  geom_path(data = trk, aes(x=x,y=y,group=id),color='blue')+
  
  # detections
  geom_point(data=filter(det,detected==1), aes(x=x_wh,y=y_wh), shape = 21, fill = 'red')+
  
  # facet
  facet_grid(day~platform)+
  
  # formatting
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())

ggsave(plot = p, filename = 'figures/1-week.png', width = 5, height = 10, units = 'in')
