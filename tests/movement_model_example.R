## movement_model_example ##
# some simple examples of how to run the movement model

# read in the model functions
source('r/rw_sim.R')
set.seed(1) # you have to read this in before the df line if you want the same random path as last time

# single whale ------------------------------------------------------------
# The `rw_sim()` function is used to simulate the movement of a single
# whale in a given behavioral state. Here's a simple example of how that
# is used:

# run the model for a feeding right whale over 24 hr period
df = rw_sim(hrs = 24, bh = 'socializing', nt = 60)

# the columns in the output data are defined as follows:
# x = x coordinate of the whale (meters)
# y = y coordinate of the whale (meters)
# t = elapsed time (seconds)
# ang = turn angle of whale (degrees)
# spd = speed of whale (meters/second)
# dst = distance whale traveled in last timestep (meters)
# dpt = cumulative distance traveled by whale (meters)
# r = range of whale from starting point
# bh = whale movement behaviour

# plot track
ggplot(df, aes(x=x,y=y))+
  geom_path()+
  coord_equal()+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(x='Easting (m)', y='Northing (m)', title='Socializing whale')

# plot range from start over time
ggplot(df, aes(x=t,y=r))+
  geom_path()

# plot distance travelled over time (should be straight line)
ggplot(df, aes(x=t,y=dpt))+
  geom_path()

# plot distribution of whale speeds (should be uniform, see line 58 of rw_sim.R)
ggplot(df, aes(x=spd))+
  geom_histogram()

# plot distribution of whale turning angles (will not be uniform,
# because it is autocorrelated/constrained by previous heading)
ggplot(df, aes(x=ang))+
  geom_histogram()

# single whale, two behaviours --------------------------------------------

df2 = rw_sim(hrs = 24, bh = 'socializing')

df_0 = rbind(df,df2)

ggplot(df_0, aes(x=x,y=y, color=bh))+
  geom_path()+
  coord_equal()

ggplot(df_0, aes(x=t,y=r))+
  geom_path()

ggplot(df_0, aes(x=t,y=dpt))+
  geom_path()

ggplot(df_0, aes(x=spd))+
  geom_histogram()

ggplot(df_0, aes(x=ang))+
  geom_histogram()

# multiple whales ---------------------------------------------------------
# The function `rw_sims()` is used to model multiple right whales. It works
# by randomly assigning starting positions of whales within a given radius (km),
# then calls the `rw_sim()` function many times in an efficient way to model
# their movement, and finally combines all the data in a nice tidy format. 
# Here's a quick example with 100 whales:

# run the model
df = rw_sims(nrws = 100, hrs = 96, bh = 'socializing', radius = 100)
# radius defines the area where the different whales can be "initialized"
# (they are randomly scattered over an area)

# plot the output
ggplot(df, aes(x=x,y=y,group=id))+
  geom_path(alpha = 0.5)+
  coord_equal()

# multiple whales with different behaviours ---------------------------------

df1 = rw_sims(nrws = 50, hrs = 96, bh = 'socializing', radius = 100)
df2 = rw_sims(nrws = 50, hrs = 96, bh = 'feeding', radius = 100)

# make a new column with the values from the `bh` and `id` column pasted together 
df1_new = transform(df1, bh_id=paste(bh, id, sep="_"))
df2_new = transform(df2, bh_id=paste(bh, id, sep="_"))

# remove id column from both dataframes to avoid confusion when plotting
df1_new <- df1_new[,-1]
df2_new <- df2_new[,-1]

all_whales <- rbind(df1_new,df2_new)

ggplot(all_whales, aes(x=x,y=y,group=bh_id, color=bh))+
  geom_path(alpha = 0.5)+
  coord_equal()

