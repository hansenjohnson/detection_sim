## detect_calls ##
# functions for right whale movement simulation

# setup -------------------------------------------------------------------

# libraries
library(tidyverse)

# functions ---------------------------------------------------------------

detection_function = function(x,L=1.045,x0=10,k=-0.3){
  # Construct a detection function using a logistic curve
  # L = maximum Y value
  # x0 = value at midpoint
  # k = logistic growth rate
  y = L/(1+exp(-1*k*(x-x0))) 
  return(y)
}
