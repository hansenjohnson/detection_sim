## run_box_surveys ##
# run box surveys and save output

# number of surveys to run for each combination of platform, n_whale, etc. 
n_surveys = 10

# numbers of right whales in box
n_whales = c(1, 5, 10, 25, 50, 75)

# dimensions of TC box (km)
tc_height = 20
tc_width = 100

# dimensions of DFO box (km)
dfo_height = 18
dfo_width = 12

# output file
ofile = 'data/processed/box_surveys.rds'

# setup -------------------------------------------------------------------

# read in functions
source('r/box_survey_functions.R')

# make data directory
data_dir = dirname(ofile)
if(!dir.exists(data_dir)){dir.create(data_dir, recursive = TRUE)}

# process -----------------------------------------------------------------

# run DFO and TC models
dfo = run_box_surveys(height = dfo_height, width = dfo_width, n_whales = n_whales, n_surveys = n_surveys) %>%
  mutate(box_type = 'DFO')
tc = run_box_surveys(height = tc_height, width = tc_width, n_whales = n_whales, n_surveys = n_surveys) %>%
  mutate(box_type = 'TC')

# combine output
df = rbind(dfo, tc)

# save
saveRDS(df, ofile)
