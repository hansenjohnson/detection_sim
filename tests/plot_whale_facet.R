## plot_whale_facet ##
# quick example of a time faceted whale track

# setup -------------------------------------------------------------------

source('r/rw_sim.R')

# process -----------------------------------------------------------------

# run rw_sim
df=rw_sim()

# convert time from s to hr
df$hr = df$t/60/60

# generate time bin
tbin = seq(from = 0, to = max(df$hr), by = 6)

# use cut to assign each row to a given time bin
df$tbin = cut(x = df$hr, breaks = tbin, include.lowest = TRUE)

# plot 
ggplot()+
  geom_path(data = df, aes(x=x,y=y))+
  geom_point(data = filter(df, call==1), aes(x=x,y=y), color = 'red')+
  facet_wrap(~tbin)+
  coord_equal()+
  theme_bw()
