## master ##
# master script to reproduce detection_sim analysis

# setup -------------------------------------------------------------------

# read in functions
source('r/box_survey_functions.R')

# process -----------------------------------------------------------------

# run surveys (large analysis done on remote machine)
source('r/run_box_surveys.R')

# calculate performance metrics
source('r/calculate_metrics.R')

#  plot -------------------------------------------------------------------

# process map data
source('r/plots/proc_map_data.R')

# plot site map
source('r/plots/f_site_map.R')

# plot detection functions
source('r/plots/plot_detection_functions.R')

# plot survey examples
source('r/plots/plot_box_survey_examples.R')

# plot survey results
source('r/plots/plot_box_survey_results.R')

# plot survey metrics
source('r/plots/plot_metrics.R')
