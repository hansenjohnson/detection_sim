## combined_tracks_example ##
# an example where multiple platforms and tracks run over the whale simulation

# setup ----------------------------------------------------------------------------

source('r/rw_sim.R')

# test and compare Del's glider with a mooring --------------------------------------

# define time resolution (s)
res = 60

# produce whale movement model and convert to km
set.seed(1)
whale = rw_sim(hrs=24*7, nt = res, sub = TRUE, x0 = 5000, y0 = 5000, bh = 'feeding')%>%
  mutate(
    x=x/1000,
    y=y/1000,
    r=r/1000
  )

# plot whale with calls and surfacing
ggplot()+
  geom_path(data = whale, aes(x=x,y=y,group=dive_index,color=surface))+
  scale_color_manual(values = c('0'='grey', '1'='black'))+
  geom_point(data=filter(whale,call==1), aes(x=x,y=y), shape = 21, fill = 'red')+
  coord_equal()+
  theme_bw()+
  theme(panel.grid=element_blank())

# produce glider track and convert to km
glider = make_track(waypoints = 'data/raw/waypoints_real_GSL_glider.csv', res = res, spd = 0.1)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )

# plot both tracks (no detections)
ggplot()+
  geom_path(data = glider, aes(x=x,y=y), color = 'blue')+
  geom_path(data = whale, aes(x=x,y=y), color = 'black')+
  geom_point(shape=1)+
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = 'bottom')+
  labs(x = 'Easting (km)', y = 'Northing (km)', fill = NULL, color = NULL)

# simulate detection capabilities of glider
acoustic_det = simulate_detections(whale_df = whale, track_df = glider, det_method = 'acoustic')

# add mooring to compare
# coordinates of detector
x_moor = 0
y_moor = 0

# make data frame using whale movement variables
df = tibble(
  time = whale$time,
  x_wh = whale$x,
  y_wh = whale$y,
  x_moor,
  y_moor,
  r_wh = sqrt((x_wh-x_moor)^2 + (y_wh-y_moor)^2),
  call = whale$call
)

# subset to only times with calls
calls = df %>% filter(call==1)

# apply detection function to the call positions to extract probabilities of detection
calls$p = detection_function(x = calls$r_wh)

# generate a binomial distribution to see if each call was detected using this probability
calls$detected = as.character(rbinom(n = nrow(calls), size = 1, prob = calls$p))

# subset to only detected calls
calls = calls %>% filter(detected==1)

# select only glider track that occurs while the whale is available
glider = glider %>% filter(glider$time<=max(whale$time))

# create new grouping variables for animations
acoustic_det$grp = seq(1,nrow(acoustic_det))
# modify time to hours for animation
acoustic_det$time = acoustic_det$time/60/60
whale$time = whale$time/60/60
glider$time = glider$time/60/60

# plot both tracks with calls detected
p = ggplot()+
  # plot glider track
  geom_path(data = glider,aes(x=x,y=y), color = 'black')+
  # plot detector position
  #geom_point(data = df,aes(x=x_moor,y=y_moor),shape=25,fill='black')+
  # plot whale track
  geom_path(data = whale,aes(x=x,y=y), color = 'grey')+
  # plot mooring detected calls
  #geom_point(data = calls,aes(x=x_wh,y=y_wh),shape=22,alpha=0.7,size=2,fill='blue')+
  # plot glider detected calls
  geom_point(data = acoustic_det,aes(x=x_wh,y=y_wh,group=grp,fill=detected,size=detected),shape=21,alpha=0.7)+
  scale_fill_manual(values = c('1'='red','0'='grey'))+
  scale_size_manual(values = c('1'=2,'0'=1))+
  # formatting
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position="bottom")+
  labs(x = 'Easting (km)', y = 'Northing (km)')

# animate
anim = p + 
  transition_reveal(along = time)+
  #ease_aes('linear')+
  ggtitle("Hours: {round(frame_along,0)}")

anim
anim_save('figures/whale_detection_animation.gif', animation=anim)

# facet wrap plot to see movement over time
# convert time from s to hr
# whale$hr = whale$time/60/60
# glider$hr = glider$time/60/60
# calls$hr = calls$time/60/60
# acoustic_det$hr = acoustic_det$time/60/60

# generate time bin
tbin = seq(from = 0, to = max(whale$time), by = 24)

# use cut to assign each row to a given time bin
whale$tbin = cut(x = whale$time, breaks = tbin, include.lowest = TRUE)
glider$tbin = cut(x = glider$time, breaks = tbin, include.lowest = TRUE)
#calls$tbin = cut(x = calls$hr, breaks = tbin, include.lowest = TRUE)
acoustic_det$tbin = cut(x = acoustic_det$time, breaks = tbin, include.lowest = TRUE)

# plot both tracks with calls detected
ggplot()+
  # plot glider track
  geom_path(data = glider,aes(x=x,y=y), color = 'black')+
  # plot detector position
  #geom_point(data = df,aes(x=x_moor,y=y_moor),shape=25,fill='black')+
  # plot whale track
  geom_path(data = whale,aes(x=x,y=y), color = 'grey')+
  # plot mooring detected calls
  #geom_point(data = calls,aes(x=x_wh,y=y_wh),shape=22,alpha=0.7,size=2,fill='blue')+
  # plot glider detected calls
  geom_point(data = acoustic_det,aes(x=x_wh,y=y_wh,fill=detected,size=detected),shape=21,alpha=0.7)+
  scale_fill_manual(values = c('1'='red','0'='grey'))+
  scale_size_manual(values = c('1'=2,'0'=1))+
  # formatting
  facet_wrap(~tbin)+
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())

# multiple whales with three platforms -------------------------------------------------

# produce whale movement model (no need to transform to kms)
set.seed(1)
whales = rw_sims(nrws = 10, hrs=24*4, nt = res, bh = 'feeding', radius=40)

# plot whale with calls and surfacing
ggplot()+
  geom_path(data = whales, aes(x=x,y=y,group=id,color=surface))+
  scale_color_manual(values = c('0'='grey', '1'='black'))+
  geom_point(data=filter(whales,call==1), aes(x=x,y=y), shape = 21, fill = 'red')+
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())

# produce plane track and convert to km
plane_2 = make_track(waypoints = 'data/raw/waypoints_plane2.csv', res = res, spd = 51)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )

# produce glider track and convert to km
glider = make_track(waypoints = 'data/raw/waypoints_plane2.csv', res = res, spd = 0.1)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )

# produce vessel track and convert to km
vessel = make_track(waypoints = 'data/raw/waypoints_plane2.csv', res = res, spd = 4)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )

# simulate detection capabilities of plane
visual_det_plane = simulate_detections(whale_df = whales, track_df = plane_2, det_method = 'visual')

# simulate detection capabilities of vessel
visual_det_vessel = simulate_detections(whale_df = whales, track_df = vessel, det_method = 'visual')

# simulate detection capabilities of glider
acoustic_det = simulate_detections(whale_df = whales, track_df = glider, det_method = 'acoustic')

# plot all tracks (no detections)
ggplot()+
  geom_path(data = glider, aes(x=x,y=y), color = 'blue')+
  geom_path(data = vessel, aes(x=x,y=y), color = 'purple')+
  geom_path(data = plane_2, aes(x=x,y=y), color = 'dark green')+
  geom_path(data = whales, aes(x=x,y=y, group=id), color = 'black')+
  geom_point(shape=1)+
  coord_equal()+
  theme_bw()

# group dive index and whale id
visual_det_plane$grp = paste(visual_det_plane$id, visual_det_plane$dive_index)
visual_det_vessel$grp = paste(visual_det_vessel$id, visual_det_vessel$dive_index)

# select only the tracks that occurs while the whales are available
glider = glider %>% filter(glider$time<=max(whales$time))

# plot both tracks with calls and surfacings detected
ggplot()+
  # plot glider track
  #geom_path(data = glider,aes(x=x,y=y), color = 'blue')+
  # plot plane track
  #geom_path(data = plane_2, aes(x=x,y=y), color = 'dark green')+
  # plot vessel track
  geom_path(data = plane_2, aes(x=x,y=y), color = 'purple')+
  # plot whale track
  geom_path(data = whales,aes(x=x,y=y, group = id), color = 'grey')+
  # plot surfacings
  geom_path(data = visual_det_vessel,aes(x=x_wh,y=y_wh, group = grp), color = 'black')+
  geom_point(data = filter(visual_det_vessel, detected == 1), aes(x=x_wh,y=y_wh),
             shape = 21, fill = 'red', size = 2, alpha = 0.7)+
  # plot calls
  # geom_point(data = filter(acoustic_det, detected == 1), aes(x=x_wh,y=y_wh),
  #            shape = 21, fill = 'red', size = 2, alpha = 0.7)+
  # formatting
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(x = 'Easting (km)', y = 'Northing (km)')

# know how many calls and surfacings were detected
acoustic_det %>% group_by(detected) %>% count()
visual_det_plane %>% group_by(detected) %>% count()
visual_det_vessel %>% group_by(detected) %>% count()

# platforms starting at different times --------------------------------------------------

# produce whale movement model (no need to transform to kms)
set.seed(1)
whales = rw_sims(nrws = 10, hrs=24*3, nt = res, bh = 'feeding', radius=20)

# plot whale with calls and surfacing
ggplot()+
  geom_path(data = whales, aes(x=x,y=y,group=id,color=surface))+
  scale_color_manual(values = c('0'='grey', '1'='black'))+
  geom_point(data=filter(whales,call==1), aes(x=x,y=y), shape = 21, fill = 'red')+
  coord_equal()+
  theme_bw()+
  theme(panel.grid=element_blank())

# produce first plane track and convert to km
plane_1 = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 51)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )

# produce second plane track and convert to km
plane_2 = make_track(waypoints = 'data/raw/waypoints_plane2.csv', res = res, spd = 50)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )

# produce glider track and convert to km
glider = make_track(waypoints = 'data/raw/waypoints_real_GSL_glider.csv', res = res, spd = 0.1)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )

# select only the tracks that occurs while the whales are available
glider = glider %>% filter(glider$time<=max(whales$time))

# start second flight one day later
plane_2$time = plane_2$time + 24*60*60

# plot all tracks (no detections)
ggplot()+
  geom_path(data = plane_1, aes(x=x,y=y), color = 'green')+
  geom_path(data = plane_2, aes(x=x,y=y), color = 'dark green')+
  geom_path(data = glider, aes(x=x,y=y), color = 'blue')+
  geom_path(data = whales, aes(x=x,y=y, group=id), color = 'black')+
  geom_point(shape=1)+
  coord_equal()+
  theme_bw()

# simulate detection capabilities of platforms
visual_det_1 = simulate_detections(whale_df = whales, track_df = plane_1, det_method = 'visual')
visual_det_2 = simulate_detections(whale_df = whales, track_df = plane_2, det_method = 'visual')
acoustic_det = simulate_detections(whale_df = whales, track_df = glider, det_method = 'acoustic')

# add column identifying flight number
visual_det_1$flight_id = '1'
visual_det_2$flight_id = '2'

# merge plane detections
visual_det = rbind(visual_det_1, visual_det_2)

# facet wrap plot to see movement over time
# convert time from s to hr
whales$hr = whales$time/60/60
plane_1$hr = plane_1$time/60/60
plane_2$hr = plane_2$time/60/60
glider$hr = glider$time/60/60
visual_det$hr = visual_det$time/60/60
acoustic_det$hr = acoustic_det$time/60/60

# generate time bin
tbin = seq(from = 0, to = max(whales$hr), by = 24)

# use cut to assign each row to a given time bin
whales$tbin = cut(x = whales$hr, breaks = tbin, include.lowest = TRUE)
plane_1$tbin = cut(x = plane_1$hr, breaks = tbin, include.lowest = TRUE)
plane_2$tbin = cut(x = plane_2$hr, breaks = tbin, include.lowest = TRUE)
glider$tbin = cut(x = glider$hr, breaks = tbin, include.lowest = TRUE)
visual_det$tbin = cut(x = visual_det$hr, breaks = tbin, include.lowest = TRUE)
acoustic_det$tbin = cut(x = acoustic_det$hr, breaks = tbin, include.lowest = TRUE)

# group dive index and whale id
visual_det$grp = paste(visual_det$id, visual_det$dive_index)

# plot both tracks with calls and surfacings detected
ggplot()+
  # plot first plane track
  geom_path(data = plane_1, aes(x=x,y=y), color = 'green')+
  # plot second plane track
  geom_path(data = plane_2, aes(x=x,y=y), color = 'dark green')+
  # plot glider track
  geom_path(data = glider, aes(x=x,y=y), color = 'blue')+
  # plot whale track
  geom_path(data = whales,aes(x=x,y=y, group = id), color = 'grey')+
  # plot surfacings
  geom_path(data = visual_det,aes(x=x_wh,y=y_wh, group = grp), color = 'black')+
  geom_point(data = filter(visual_det, detected == 1), aes(x=x_wh,y=y_wh), 
             shape = 24, fill = 'red', size = 2, alpha = 0.7)+
  # plot calls
  geom_point(data = filter(acoustic_det, detected == 1), aes(x=x_wh,y=y_wh),
             shape = 21, fill = 'red', size = 2, alpha = 0.7)+
  # formatting
  facet_wrap(~tbin)+
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(x='Easting (km)', y='Northing (km)')

# know how many surfacings were detected
visual_det %>% group_by(detected) %>% count()
acoustic_det %>% group_by(detected) %>% count()

# compare plane and glider swith same track -------------------------------------------

# produce whale movement model (no need to transform to kms)
set.seed(1)
whales = rw_sims(nrws = 5, hrs=24, nt = res, bh = 'feeding', radius=5)

# plot whale with calls and surfacing
ggplot()+
  geom_path(data = whales, aes(x=x,y=y,group=id,color=surface))+
  scale_color_manual(values = c('0'='grey', '1'='black'))+
  geom_point(data=filter(whales,call==1), aes(x=x,y=y), shape = 21, fill = 'red')+
  coord_equal()+
  theme_bw()

# produce glider track and convert to km
glider = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 0.1)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )

# produce first plane track and convert to km
plane = make_track(waypoints = 'data/raw/waypoints_plane.csv', res = res, spd = 51)%>%
  mutate(
    x=x/1000,
    y=y/1000
  )

# plot all tracks (no detections)
ggplot()+
  geom_path(data = glider, aes(x=x,y=y), color = 'light blue')+
  geom_path(data = plane, aes(x=x,y=y), color = 'pink')+
  geom_path(data = whales, aes(x=x,y=y, group=id), color = 'black')+
  geom_point(shape=1)+
  coord_equal()+
  theme_bw()

# simulate detection capabilities of planes
acoustic_det = simulate_detections(whale_df = whales, track_df = glider, det_method = 'acoustic')
visual_det = simulate_detections(whale_df = whales, track_df = plane, det_method = 'visual')

# facet wrap plot to see movement over time
# convert time from s to hr
whales$hr = whales$time/60/60
glider$hr = glider$time/60/60
plane$hr = plane$time/60/60
visual_det$hr = visual_det$time/60/60
acoustic_det$hr = acoustic_det$time/60/60

# generate time bin
tbin = seq(from = 0, to = max(whales$hr), by = 4)

# use cut to assign each row to a given time bin
whales$tbin = cut(x = whales$hr, breaks = tbin, include.lowest = TRUE)
glider$tbin = cut(x = glider$hr, breaks = tbin, include.lowest = TRUE)
plane$tbin = cut(x = plane$hr, breaks = tbin, include.lowest = TRUE)
visual_det$tbin = cut(x = visual_det$hr, breaks = tbin, include.lowest = TRUE)
acoustic_det$tbin = cut(x = acoustic_det$hr, breaks = tbin, include.lowest = TRUE)

# group dive index and whale id
visual_det$grp = paste(visual_det$id, visual_det$dive_index)

# plot both tracks with calls and surfacings detected
ggplot()+
  # plot first plane track
  geom_path(data = glider, aes(x=x,y=y), color = 'light blue')+
  # plot second plane track
  geom_path(data = plane, aes(x=x,y=y), color = 'purple')+
  # plot whale track
  geom_path(data = whales,aes(x=x,y=y, group = id), color = 'grey')+
  # plot surfacings
  geom_path(data = visual_det,aes(x=x_wh,y=y_wh, group = grp), color = 'black')+
  geom_point(data = filter(visual_det, detected == 1), aes(x=x_wh,y=y_wh), 
             shape = 21, fill = 'blue', size = 2, alpha = 0.7)+
  # plot calls
  geom_point(data = filter(acoustic_det, detected == 1), aes(x=x_wh,y=y_wh), 
             shape = 21, fill = 'red', size = 2, alpha = 0.7)+
  # formatting
  facet_wrap(~tbin)+
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())

