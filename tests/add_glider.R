## add_glider ##
# functions for moving glider with detection capabilities

# setup -------------------------------------------------------------------

set.seed(1)

# libraries
library(ggplot2)
source('tests/make_track.R')

# add detection capabilities -------------------------------------------------

#rename whale movement model table and lose unwanted variables 
colnames(wh) = c('x_wh', 'y_wh', 'time', 'ang', 'spd', 'dst', 'dpt', 'r', 'bh', 'call')
wh = wh %>% transmute(x_wh, y_wh, time, call)

#rename track movement model table 
colnames(trk) = c('x_dt', 'y_dt', 'time')

# make data frame using whale movement variables
df = merge(wh, trk, by='time', all.x=TRUE)
df$r_wh = sqrt((df$x_wh-df$x_dt)^2 + (df$y_wh-df$y_dt)^2)

# subset to only times with calls
calls = df %>% filter(call==1)

# apply detection function to the call positions to extract probabilities of detection
calls$p = detection_function(x = calls$r_wh)

# generate a binomial distribution to see if each call was detected using this probability
calls$detected = as.character(rbinom(n = nrow(calls), size = 1, prob = calls$p))

dev.off()

# plot to check
p = ggplot()+
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

ggsave(filename = 'figures/track_with_whale_calls.png', plot = p, width = 8, height = 4, units = 'in', dpi = 300)

