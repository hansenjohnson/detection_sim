# quick test to add call rate to sim model

# setup -------------------------------------------------------------------

# read in model function
## add_call_rate ##
# quick test to add call rate to rw sim model

source('r/rw_sim.R')

# run model ---------------------------------------------------------------

# run a simple model
hrs = 96
df_single = rw_sim(hrs = hrs, bh = 'feeding', sub = TRUE, nt = 60)

# fixed call rate ---------------------------------------------------------

# calculate the time interval
#dt = df$t[2] - df$t[1]

# call rate (calls/whale/hr)
#cr_hr = 50

# convert from hours to seconds
#cr_sec = cr_hr/60/60

# calculate likelihood of call in timestep
#cr_p = cr_sec*dt # KEY - this must be less than 1!

# warn if cr_p is NOT less than 1
#if(cr_p>1){
#  warning('Liklihood of call in timestep exceeds 1! Increase the time resolution or decrease the call rate to avoid errors')
#}

# generate a binomial distribution using this probability
#df$call = rbinom(n = nrow(df), size = 1, prob = cr_p)

# plot to check
#ggplot()+
#  geom_path(data=df,aes(x=x,y=y))+
#  geom_point(data=filter(df,call==1),aes(x=x,y=y),shape=21, fill='red')+
#  coord_equal()+
#  theme_bw()

# calculate the number of calls
#n_calls = df %>% 
#  filter(call==1) %>%
#  nrow()

# calculate the observed call rate
#cr_obs = n_calls/hrs

# print a message comparing observed and expected
#message('Expected call rate: ', cr_hr)
#message('Observed call rate: ', round(cr_obs,2)) # second argument selects how many decimal places

# call rate from normal distribution --------------------------------------

# calculate the time interval
#dt = df$t[2] - df$t[1]

# mean call rate (calls/whale/hr)
#cr_mn_hr = 0.25

# standard deviation of call rate
#cr_sd_hr = 0.001

# generate a normal distribution to assign call rate in each timestep
#cr_hr = rnorm(n = nrow(df), mean = cr_mn_hr, sd = cr_sd_hr)

# quick check of normality
# hist(cr_hr) # should be a nice normal curve
# mean(cr_hr) # should match cr_mn_hr
# sd(cr_hr) # should match cr_sd_hr

# convert from hours to seconds
#cr_sec = cr_hr/60/60

# calculate likelihood of call in timestep
#cr_p = cr_sec*dt # KEY - this must be less than 1!

# warn if cr_p is NOT less than 1
#if(max(cr_p)>1){
#  warning('Liklihood of call in timestep exceeds 1! Increase the time resolution or decrease the call rate to avoid errors')
#}

# generate a binomial distribution using this probability
#df$call = rbinom(n = nrow(df), size = 1, prob = cr_p)

# plot to check
ggplot()+
  geom_path(data=df,aes(x=x,y=y))+
  geom_point(data=filter(df,call==1),aes(x=x,y=y),shape=21, fill='red')+
  coord_equal()+
  theme_bw()

# calculate the number of calls
#n_calls = df %>% 
  filter(call==1) %>%
  nrow()

# calculate the observed call rate
#cr_obs = n_calls/hrs

# print a message comparing observed and expected
#message('Expected call rate: ', cr_mn_hr)
#message('Observed call rate: ', round(x = cr_obs, digits = 2))

# test function with calling rate incorporated ----------------------------

# run a simple model
df_multiple = rw_sims(hrs = hrs, bh = 'feeding', nt = 60)

# plot to check
ggplot()+
  geom_path(data=df_multiple, aes(x=x,y=y,group=id))+
  geom_point(data=filter(df,call==1),aes(x=x,y=y),shape=21, fill='red')+
  geom_path(alpha = 0.5)+
  coord_equal()
