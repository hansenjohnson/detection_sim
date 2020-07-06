## distributions_in_r ##
# overview of some ways to generate distributions in R

# setup -------------------------------------------------------------------

# libraries
library(tidyverse)

# Making distributions requires generating random numbers. This can cause
# your results to change each time you repeat the analysis. You can avoid
# that using the `set.seed()` function, so R generates the same random 
# numbers each time the analysis is repeated.
set.seed(1)

# total number of observations
n = 1000

# generate a linear sequence of n values from 1 to 100
x = seq(from = 1, to = 100, length.out = n)

# uniform -----------------------------------------------------------------
# https://en.wikipedia.org/wiki/Uniform_distribution_(continuous)
# a uniform distribution describes a situation where there is 
# an equal probability of an observation occuring between two bounds 

# generate a unform distribution of n values between 5 and 10
y = runif(n = n, min = 5, max = 10)

# store in a tibble
uniform = tibble(x,y)
# we created a table with two columns, x and y, each with 1000 (n) observations; 
# x has numbers from 1-100 and y has numbers from 5-10

# plot data
ggplot(uniform, aes(x=x,y=y))+
  geom_point(shape=1)

# plot histogram
ggplot(uniform, aes(x=y))+
  geom_histogram(color = 'black')
# if you increase n, the histogram becomes more and more uniform

# normal ------------------------------------------------------------------
# https://en.wikipedia.org/wiki/Normal_distribution
# a normal distribution describes a situation where the probability of an
# observation is a gaussian curve with a given mean and standard deviation

# generate a normal distribution of n values with a mean of 10 and standard
# deviation of 2
y = rnorm(n = n, mean = 10, sd = 2)

# store in a tibble
normal = tibble(x,y)

# plot
ggplot(normal, aes(x=x,y=y))+
  geom_point(shape=1)

# plot histogram
ggplot(normal, aes(x=y))+
  geom_histogram(color = 'black')
# the bigger n is, the more normal the histogram

# binomial ----------------------------------------------------------------
# https://en.wikipedia.org/wiki/Binomial_distribution
# a binomial distribution describes the probability that a trial will
# succeed (1) or fail (0)

# generate a binomial distribution of n observations and a probability of
# success (1) of 0.75
y = rbinom(n = n, size = 1, prob = 0.75)

# store in a tibble
binom = tibble(x,y)

# plot
ggplot(binom, aes(x=x,y=y))+
  geom_point(shape=1)

# plot histogram
ggplot(binom, aes(x=y))+
  geom_histogram(color = 'black')

