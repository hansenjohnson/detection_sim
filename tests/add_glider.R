## add_glider ##
# functions for moving glider with detection capabilities

# setup -------------------------------------------------------------------

set.seed(1)

# libraries
source('tests/make_track.R')
source('r/detect_calls.R')

# add detection capabilities -------------------------------------------------

# make data frame using whale movement variables
df = tibble(
  t = wh$t,
  x_wh = wh$x,
  y_wh = wh$y,
  x_dt = trk$x,
  y_dt = trk$y,
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
  # plot glider track
  geom_path(data = df,aes(x=x_dt,y=y_dt), color = 'black')+
  # plot whale track
  geom_path(data = df,aes(x=x_wh,y=y_wh), color = 'grey')+
  # plot calls
  geom_point(data = calls,aes(x=x_wh,y=y_wh,fill=detected,size=detected),shape=21,alpha=0.7)+
  scale_fill_manual(values = c('1'='red','0'='grey'))+
  scale_size_manual(values = c('1'=2,'0'=1))+
  # formatting
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank())
