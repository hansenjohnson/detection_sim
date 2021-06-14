## buffer_debug ##
# tests to debug the errors having to do with buffering the tracks

# input -------------------------------------------------------------------

ofile = 'tests/rpas/surveys.rds'

# setup -------------------------------------------------------------------

set.seed(123)
source('tests/rpas/box_survey_functions.R')

# process -----------------------------------------------------------------

# run surveys
df = run_box_surveys(height = 18, width = 12, n_surveys = 10, n_whales = c(1,5,10,15,30,60), 
                     whales_parallel = FALSE, survey_parallel = FALSE)

# save
saveRDS(df, file = ofile)
df = readRDS(ofile)
