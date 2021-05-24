## detect_per_surfacing ##
# example for counting visual detections to a per surfacing basis

library(tidyverse)
source('tests/rpas/box_survey_functions.R')
set.seed(123)

# run whale movement model
platform = 'vessel'
nrws = 10
hrs = 6
bh = 'feeding'
res = 2.5
height = 18
width = 12
run_parallel = FALSE

# simulate a transits of a given platform of a box containing nrws 

# center grid on zero
xmin = 0-width/2
xmax = 0+width/2
ymin = 0-height/2
ymax = 0+height/2

# create survey track
trk = simulate_track(platform=platform,res=res,ymax,ymin,xmax,xmin)
max_time = max(trk$time,na.rm = T)
nhrs = ceiling(max_time/60/60)

# simulate whales and reflect
rws = rw_sims(
  nrws = nrws,
  hrs = nhrs,
  bh = bh,
  dt = res,
  nt = res,
  xmin = xmin,
  xmax = xmax,
  ymin = ymin,
  ymax = ymax,
  run_parallel = run_parallel 
) %>% 
  filter(time <= max_time) %>%
  reflect_rws(., ymax, ymin, xmax, xmin)

# simulate detections
det = simulate_detections(whale_df = rws, track_df = trk, platform = platform)

if('dive_index' %in% colnames(det)){
  det = det %>%
    mutate(
      id = as.numeric(id),
      detected = as.numeric(detected)
      ) %>%
    group_by(id,dive_index) %>%
    summarize(
      surface = unique(surface),
      p = mean(p),
      time = mean(time),
      x_wh = mean(x_wh),
      y_wh = mean(y_wh),
      detected = sum(detected),
      .groups = 'drop'
    )
  
  # convert to binary (0,1) detection
  det$detected[det$detected>0]=1
}

