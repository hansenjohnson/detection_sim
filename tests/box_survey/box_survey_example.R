## box_survey_example ##
# example to demonstrate the box survey idea and components

# setup -------------------------------------------------------------------

source('tests/box_survey/box_survey_functions.R')
set.seed(123)
#reproduce a particular sequence of 'random' numbers

# define the limits of a box (units in km)
xmin = -3
xmax = 3
ymin = -3
ymax = 3

# reflecting whales -------------------------------------------------------

# simulate movement
rws1 = rw_sims(nrws = 3, hrs = 3, bh = 'feeding', radius = xmax)

# reflect the whales off the walls (to keep them within the box)
rws2 = reflect_rws(rws = rws1,ymax = ymax,ymin = ymin,xmax = xmax,xmin = xmin)

# format for plotting
rws1$type = 'original'
rws2$type = 'reflected'
rws = rbind(rws1,rws2)

# plot
ggplot()+
  annotate('rect',xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=NA,color='grey')+
  geom_path(data=rws,aes(x=x,y=y,group=id,color=surface))+
  geom_point(data=filter(rws,call==1),aes(x=x,y=y),shape=1)+
  scale_color_manual(values = c('0'='grey','1'='black'))+
  facet_wrap(~type)+
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = "none")

# simulate tracks ---------------------------------------------------------

# randomly simulate transit across the box (from right to left)
trk = simulate_track(platform = 'glider', ymax = ymax, ymin = ymin, xmax = xmax, xmin = xmin)

# plot
ggplot()+
  annotate('rect',xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=NA,color='grey')+
  geom_path(data=rws2,aes(x=x,y=y,group=id,color=surface))+
  geom_point(data=filter(rws2,call==1),aes(x=x,y=y),shape=1)+
  geom_path(data=trk,aes(x=x,y=y), color = 'blue')+
  scale_color_manual(values = c('0'='grey','1'='black'))+
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = "none")

# simulate detections -----------------------------------------------------

# simulate detections (same as previous)
det = simulate_detections(whale_df = rws2, track_df = trk, det_method = 'acoustic')

# plot
ggplot()+
  annotate('rect',xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=NA,color='grey')+
  geom_path(data=rws2,aes(x=x,y=y,group=id,color=surface))+
  geom_point(data=filter(rws2,call==1),aes(x=x,y=y),shape=1)+
  geom_point(data=filter(det,detected==1),aes(x=x_wh,y=y_wh),shape=21,fill='red')+
  geom_path(data=trk,aes(x=x,y=y), color = 'blue')+
  scale_color_manual(values = c('0'='grey','1'='black'))+
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = "none")

# simulate many surveys ---------------------------------------------------

# many surveys with a single platform
df = run_box_survey(
  height=18,        # height of box (km)
  width=12,         # width of box (km)
  nrws=5,           # number of right whales in box
  n_surveys=5,      # number of surveys of the box (transits)
  bh='feeding',     # whale behavior
  platform='glider',# survey platform
  include_data=T    # include raw data in output (only do this for small runs)
)

# view summary table
print(df)

# plot a given survey
df$plot[1] # change this number to show a plot of each survey

# many surveys with multiple platforms
df2 = run_box_surveys(
  height=18,        # height of box (km)
  width=12,         # width of box (km)
  nrws=1,           # number of right whales in box
  n_surveys=5,      # number of surveys of the box (transits)
  bh='feeding'      # whale behavior
)

# view summary table
print(df2)
