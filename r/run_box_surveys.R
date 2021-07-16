## run_box_surveys ##
# run box surveys and save output

# input -------------------------------------------------------------------

# number of surveys to run for each combination of platform, n_whale, etc.
n_surveys = 1000

# numbers of right whales in box
n_whales = c(seq(1, 10, 1), seq(15, 65, 5))

# dimensions of TC box (km)
tc_height = 20
tc_width = 100

# dimensions of DFO box (km)
dfo_height = 18
dfo_width = 12

# output file
ofile = 'data/processed/box_surveys.rds'

# setup -------------------------------------------------------------------

set.seed(123)
# read in functions
source('r/box_survey_functions.R')

# make data directory
data_dir = dirname(ofile)
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

# process -----------------------------------------------------------------

message('\n###############################')
message('## NARW DETECTION SIMULATION ##')
message('###############################\n')

message('Start time: ', Sys.time(), ' (', Sys.timezone(), ')')

# run TC simulation
message('\nRunning TC simulation')
tc = run_box_surveys(
  height = tc_height,
  width = tc_width,
  n_surveys = n_surveys,
  n_whales = n_whales,
  whales_parallel = FALSE,
  survey_parallel = TRUE
) %>%
  mutate(box_type = 'TC')

# run DFO simulation
message('\nRunning DFO simulation')
dfo = run_box_surveys(
  height = dfo_height,
  width = dfo_width,
  n_surveys = n_surveys,
  n_whales = n_whales,
  whales_parallel = FALSE,
  survey_parallel = TRUE
) %>%
  mutate(box_type = 'DFO')

# combine output
df = bind_rows(dfo, tc)

# save
saveRDS(df, ofile)

message('\nData saved as: ', ofile)

message('\n#############################')
message('## SIMULATION COMPLETE :)  ##')
message('#############################\n')
