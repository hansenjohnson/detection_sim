## plot_metrics ##
# make a plot of all 5 metrics

# input -------------------------------------------------------------------

ifile = 'data/processed/metrics.rds'
ofile = 'figures/metrics.png'

# setup -------------------------------------------------------------------

library(tidyverse)

# process -----------------------------------------------------------------

# read in data
df = readRDS(ifile)

# pivot df for plotting
dfl = df %>%
  pivot_longer(det_per_hour:cost_per_det, names_to = 'vars', values_to = 'vals') %>%
  dplyr::select(platform, n_whales, box_type, vars, vals)

# subset for plotting
dfl_dfo = dfl %>% dplyr::filter(box_type == 'DFO')
dfl_tc = dfl %>% dplyr::filter(box_type == 'TC')

# plot
p = ggplot()+
  geom_path(data = dfl_dfo, aes(x = n_whales, y = vals, group = platform, color = platform, linetype = box_type))+
  geom_path(data = dfl_tc, aes(x = n_whales, y = vals, group = platform, color = platform, linetype = box_type))+
  facet_wrap(~vars, scales = 'free_y')+
  theme_bw()

# save
ggsave(filename = ofile, plot = p, width = 8, height = 6, units = 'in', dpi = 300)
