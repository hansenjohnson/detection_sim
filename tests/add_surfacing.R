## add_surfacing ##
# test to add surfacing behaviour to movement model

# input -------------------------------------------------------------------

# time resolution
res = 60

# dive time (s)
dtime_mean = 720

# standard dev dive time (s)
dtime_sd = 180

# surface time (s)
stime_mean = 300

# standard dev surface time (s)
stime_sd = 60

# setup -------------------------------------------------------------------

source('r/rw_sim.R')

# process -----------------------------------------------------------------

# run right whale model
df = rw_sim(nt = res, sub = TRUE)

# estimate cycle duration
cycle_dur = dtime_mean+stime_mean

# number of dive cycles to simulate (maximum estimate)
n_cycles = ceiling(max(df$time)/cycle_dur)*2

# generate distributions of surfacing and dive times
stimes = rnorm(n = n_cycles, mean = stime_mean, sd = stime_sd)
dtimes = rnorm(n = n_cycles, mean = dtime_mean, sd = dtime_sd)

# generate table with alternating diving/surfacing and associated metadata
cyc = tibble(
  dive_dur = c(0, round(c(rbind(dtimes,stimes)),0)),
  dive_time = cumsum(dive_dur),
  dive_index = seq(from=1, to = length(dive_time), by = 1),
  surface = as.character(rep(c(0,1), length.out = length(dive_time)))
)

# bin whale movement df by dive time
df$dive_index = cut(x = df$time, breaks = cyc$dive_time, labels = F, include.lowest = TRUE)

# merge dfs to include dive cycle info in movement df
df = left_join(x = df, y = cyc, by = 'dive_index')

# plot to check
ggplot()+
  geom_path(data = df, aes(x=x,y=y,group=dive_index,color=surface))+
  scale_color_manual(values = c('0'='grey', '1'='black'))+
  geom_point(data=filter(df,call==1), aes(x=x,y=y), shape = 21, fill = 'red')+
  coord_equal()+
  theme_bw()
