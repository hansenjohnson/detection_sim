## plot_metrics ##
# make a plot of all 5 metrics

# input -------------------------------------------------------------------

ifile = 'data/processed/metrics.rds'

# setup -------------------------------------------------------------------

library(tidyverse)

platform_cols = c('Slocum glider' = 'blue', 'Aircraft' = 'red', 
                  'Vessel' = 'black', 'RPAS' = 'lightslategrey')

# process -----------------------------------------------------------------

# read in data
df = readRDS(ifile)

df$platform = recode(df$platform, slocum = "Slocum glider", plane = "Aircraft", 
                     vessel = "Vessel", rpas = "RPAS")

# pivot df for plotting
dfl = df %>%
  pivot_longer(det_per_hour:cost_per_det, names_to = 'vars', values_to = 'vals') %>%
  dplyr::select(platform, n_whales, box_type, vars, vals)

# define factors for plotting order
dfl$vars = factor(dfl$vars, levels = c("det_per_hour", "det_per_dist", 
                                               "det_per_area", "det_area_time", 
                                               "cost_per_det"), ordered = TRUE)

# define factors for plotting order
dfl$platform = factor(dfl$platform, levels = c("Aircraft","RPAS","Vessel","Slocum glider"), ordered = TRUE)

# prepare metric labels for plotting
my_labeller = as_labeller(c(cost_per_det="C[d]", 
                            det_area_time="D[ta]", 
                            det_per_area="D[a]",
                            det_per_dist="D[d]",
                            det_per_hour="D[t]"),
                           default = label_parsed)

# subset for plotting
dfl_dfo = dfl %>% dplyr::filter(box_type == 'DFO')
dfl_tc = dfl %>% dplyr::filter(box_type == 'TC')

# plot
p = ggplot()+
  geom_path(data = dfl_dfo, aes(x = n_whales, y = vals, group = platform, color = platform))+
  scale_color_manual(values = platform_cols)+
  labs(x='Number of whales', y='Performance metric value', color = ' Platforms')+
  facet_wrap(~vars, scales = 'free', labeller=my_labeller)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())
p

q = ggplot()+
  geom_path(data = dfl_tc, aes(x = n_whales, y = vals, group = platform, color = platform))+
  scale_color_manual(values = platform_cols)+
  labs(x='Number of whales', y='Performance metric value', color = ' Platforms')+
  facet_wrap(~vars, scales = 'free', labeller=my_labeller)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())
q

# save
ggsave(filename = 'figures/metrics_dfo.png', plot = p, width = 8, height = 6, units = 'in', dpi = 300)
ggsave(filename = 'figures/metrics_tc.png', plot = q, width = 8, height = 6, units = 'in', dpi = 300)

# make smaller tables with metrics for manuscript
df_dfo = df %>% dplyr::filter(box_type== 'DFO', n_whales %in% c(1,10,30,60))
df_dfo = df_dfo %>% 
  dplyr::select(platform, n_whales, 
                det_per_hour,det_per_dist, det_per_area, det_area_time, cost_per_det)
metrics_dfo = data.frame(t(df_dfo))

df_tc = df %>% dplyr::filter(box_type== 'TC', n_whales %in% c(1,10,30,60))
df_tc = df_tc %>% 
  dplyr::select(platform, n_whales, 
                det_per_hour,det_per_dist, det_per_area, det_area_time, cost_per_det)
metrics_tc = data.frame(t(df_tc))

# save
write.csv(metrics_dfo, 'data/processed/metrics_table_dfo.csv')
write.csv(metrics_tc, 'data/processed/metrics_table_tc.csv')
