## master ##
# master script to reproduce detection_sim analysis

# setup -------------------------------------------------------------------

# read in functions
source('r/rw_sim.R')

# process -----------------------------------------------------------------

# run 1 week model
source('r/run_one_week.R')

# run diagnostics
source('r/run_diagnostics.R')

#  plot -------------------------------------------------------------------

# detection function
source('r/plot_detection_functions.R')

# 1 week example
source('r/plot_one_week.R')
