## choose_call_dist ##
# choose a probability distribution for call rate
# follow here: https://rpubs.com/eraldagjika/715261

# setup -------------------------------------------------------------------

library(tidyverse)
library(fitdistrplus)

# process -----------------------------------------------------------------

# read in data
df = readRDS('data/processed/call_production_rate.rds') %>%
  filter(upcall_production_rate <= 2) # remove outliers

ggplot(df)+
  geom_histogram(aes(x = upcall_production_rate))

ggplot(df)+
  geom_density(aes(x = upcall_production_rate))

# isolate call rate data
cr = df$upcall_production_rate

# plot distribution
plotdist(cr, histo = TRUE, demp = TRUE)

# choose distribution -----------------------------------------------------

# check
par(mfrow = c(1, 1))
descdist(cr, boot = 1000, discrete = F)

# fit models
unif.f = fitdist(cr, 'unif')
norm.f = fitdist(cr, 'norm')
exp.f = fitdist(cr, 'exp')
logis.f = fitdist(cr, 'logis')

par(mfrow = c(2, 2))
plot.legend <- c("Uniform", "Normal","Exponential", "Logistic")
denscomp(list(unif.f,norm.f,exp.f,logis.f), legendtext = plot.legend)
qqcomp(list(unif.f,norm.f,exp.f,logis.f), legendtext = plot.legend)
cdfcomp(list(unif.f,norm.f,exp.f,logis.f), legendtext = plot.legend)
ppcomp(list(unif.f,norm.f,exp.f,logis.f), legendtext = plot.legend)

# compare AIC
unif.f$aic
norm.f$aic
exp.f$aic # best
logis.f$aic

# The best model is the exponential, using rate of 4.271893

# plot --------------------------------------------------------------------

tmp = tibble(x=rexp(1e3, rate = 4.271893))

ggplot()+
  geom_density(data = df, aes(x=upcall_production_rate), color = 'black')+
  geom_density(data = tmp, aes(x=x), color = 'blue')

