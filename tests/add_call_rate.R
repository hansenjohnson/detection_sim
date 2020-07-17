# quick test to add call rate to sim model

# setup -------------------------------------------------------------------

# read in model function
source('r/rw_sim.R')

# run model ---------------------------------------------------------------

# run a simple model
hrs = 96
df = rw_sim(hr=96,bh='feeding',sub=TRUE,nt=60)

# plot to check
# ggplot()+
#   geom_path(data=df,aes(x=x,y=y,color=t))+
#   coord_equal()+
#   theme_bw()

# fixed call rate ----------------------------------------------------------

# calculate time interval
dt = df$t[2] - df$t[1]

# call rate (call/rate/hr)
cr_hr = 0.25

# converting from hours to seconds
cr_sec = cr_hr/60/60

# calculate likelihood of call in timestep
cr_p = cr_sec*dt # needs to be less than 1

# warn if cr_p is NOT less than 1
if(cr_p>1){
  warning('likelihood of call in timestep exceeds 1! Increase the time resolution or decrease the call rate to avoid errors')
}

# generate a binomial distribution using this probability
df$call = rbinom(n=nrow(df),size=1,prob=cr_p)

# plot to check
ggplot()+
  geom_path(data=df,aes(x=x,y=y))+
  geom_point(data=filter(df,call==1), aes(x=x,y=y), shape=21,fill='red')+
  coord_equal()+
  theme_bw()
  
# calculate the number of calls
cr_calls = df %>%
  filter(call==1) %>%
  nrow()
  
# calculate the observed call rate
cr_obs = n_calls/hrs

# print a message comparing observed and expected
message('Expected call rate:', cr_hr)
message('Observed call rate:', round(cr_obs,2)) # second argument selects how many decimal places

# call rate from normal distribution ---------------------------------------

# calculate time interval
dt = df$t[2] - df$t[1]

# call rate (call/rate/hr)
cr_mn_hr = 0.25

# standard deviation of call rate
cr_sd_hr = 0.001

# generate a normal distribution to assign call rate in each timestep
cr_hr = rnorm(nrow(df),mean=cr_mn_hr,sd=cr_sd_hr)

# quick check for normality
hist(cr_hr) # should be a nice normal curve
mean(cr_hr) # should be close to cr_mn_hr
sd(cr_hr) # should be close to cr_sd_hr

# converting from hours to seconds
cr_sec = cr_hr/60/60

# calculate likelihood of call in timestep
cr_p = cr_sec*dt # needs to be less than 1

# warn if cr_p is NOT less than 1
if(max(cr_p>1)){
  warning('likelihood of call in timestep exceeds 1! Increase the time resolution or decrease the call rate to avoid errors')
}

# generate a binomial distribution using this probability
df$call = rbinom(n=nrow(df),size=1,prob=cr_p)

# plot to check
ggplot()+
  geom_path(data=df,aes(x=x,y=y))+
  geom_point(data=filter(df,call==1), aes(x=x,y=y), shape=21,fill='red')+
coord_equal()+
  theme_bw()

# calculate the number of calls
n_calls = df %>%
  filter(call==1) %>%
  nrow()

# calculate the observed call rate
cr_obs = n_calls/hrs

# print a message comparing observed and expected
message('Expected call rate:', cr_mn_hr)
message('Observed call rate:', round(cr_obs,2))  
