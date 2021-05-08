## rpas_survey ##
# test to add rpas surveys

# setup -------------------------------------------------------------------

set.seed(123)
source('tests/rpas/box_survey_functions.R')

# check -------------------------------------------------------------------

# simulate rpas surveys and return data
df_rpas = box_surveys(
  platform = 'rpas',
  height = 18,
  width = 12,
  nrws = 60,
  n_surveys = 3,
  bh = 'feeding',
  whales_parallel = TRUE,
  survey_parallel = FALSE,
  include_data = TRUE
)

# show plot
df_rpas$plot[[2]]

# simulate plane surveys and return data
df_plane = box_surveys(
  platform = 'plane',
  height = 18,
  width = 12,
  nrws = 60,
  n_surveys = 3,
  bh = 'feeding',
  whales_parallel = TRUE,
  survey_parallel = FALSE,
  include_data = TRUE
)

# show plot
df_plane$plot[[2]]

# process -----------------------------------------------------------------

# run surveys
df = run_box_surveys(height = 18, width = 12, n_surveys = 10, n_whales = c(1,5,10,15,30,60))

# save
saveRDS(df, 'tests/rpas/surveys.rds')

# summarize
out = df %>%
  group_by(platform, n_whales) %>%
  summarize(
    platform = unique(platform),
    n_whales = unique(n_whales),
    behavior = unique(behavior),
    transits = length(unique(run)),
    transit_time = mean(transit_time),
    transit_dist = mean(transit_dist),
    transits_with_detections = sum(detected),
    transit_p = transits_with_detections/transits,
    .groups = 'drop'
  )

# check 
out

# plot
p = ggplot()+
  geom_path(data=out,aes(x=n_whales,y=transit_p,color=platform,group=platform))+
  scale_color_manual(values = c('slocum' = 'blue', 'plane' = 'red', 'vessel' = 'darkslategrey', 'wave' = 'cornsilk3', 'rpas' = 'purple'))+
  labs(x = 'Number of whales', 
       y = 'Probability of detection per transit', 
       color = 'Platform')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())
p
