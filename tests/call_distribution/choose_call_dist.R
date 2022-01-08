## choose_call_dist ##
# choose a probability distribution for call rate
# follow here: https://rpubs.com/eraldagjika/715261

# setup -------------------------------------------------------------------

library(tidyverse)
library(fitdistrplus)

install.packages("AICcmodavg")
library(AICcmodavg)

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
#The function descdist() provides a skewness-kurtosis graph to help to choose the best candidate(s) to fit a given dataset.
#If we want to use it for discrete distributions we may use argument discrete=TRUE.

# fit models
unif.f = fitdist(cr, 'unif') #uniform distribution
norm.f = fitdist(cr, 'norm') #normal distribution
exp.f = fitdist(cr, 'exp') #exponential distribution
logis.f = fitdist(cr, 'logis') #logistical distribution
# note rates that come out of these (used when modelling calls later)

par(mfrow = c(2, 2))
plot.legend <- c("Uniform", "Normal","Exponential", "Logistic")
denscomp(list(unif.f,norm.f,exp.f,logis.f), legendtext = plot.legend)
qqcomp(list(unif.f,norm.f,exp.f,logis.f), legendtext = plot.legend)
cdfcomp(list(unif.f,norm.f,exp.f,logis.f), legendtext = plot.legend)
ppcomp(list(unif.f,norm.f,exp.f,logis.f), legendtext = plot.legend)
#We can use these graphical test to compare the fitted distribiutions with the real observations. 
#We expect the distributions to follow the patterns of the real observations in the histogram, QQ-plot, CDF plot and also PP-plot.

# compare AIC
unif.f$aic
norm.f$aic
exp.f$aic # best
logis.f$aic
# The Akaike information criterion (AIC) is an estimator of prediction error 
# and thereby relative quality of statistical models for a given set of data
# The best-fit model according to AIC is the one that explains the greatest amount of variation 
# using the fewest possible independent variables

# choose best model (first one listed)
models <- list(unif.f, norm.f, exp.f, logis.f)
model.names <- c('uniform', 'normal', 'exponential', 'logistical')
aictab(cand.set = models, modnames = model.names)

# The best model is the exponential, using rate of 4.271893

# plot --------------------------------------------------------------------

tmp = tibble(x=rexp(1e3, rate = 4.271893))

ggplot()+
  geom_density(data = df, aes(x=upcall_production_rate), color = 'black')+
  geom_density(data = tmp, aes(x=x), color = 'blue')

citation("fitdistrplus")
citation("AICcmodavg")
