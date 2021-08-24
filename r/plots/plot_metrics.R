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

# pivot df and limit whale number for plotting
dfl = df %>%
  pivot_longer(det_per_hour:cost_per_det, names_to = 'vars', values_to = 'vals') %>%
  dplyr::select(platform, n_whales, box_type, vars, vals) %>%
  dplyr::filter(n_whales <= 20)

# define factors for plotting order
dfl$vars = factor(dfl$vars, levels = c("det_per_hour", "det_per_dist", 
                                               "det_per_area", "det_area_time", 
                                               "cost_per_det"), ordered = TRUE)

# define factors for plotting order
dfl$platform = factor(dfl$platform, levels = c("Aircraft","RPAS","Vessel","Slocum glider"), ordered = TRUE)

# prepare metric labels for plotting
my_labeller = as_labeller(c(cost_per_det=paste("C[d]  (CAD/detection)"), 
                            det_area_time="D[ta]   (detections/hr/km^{2})", 
                            det_per_area="D[a]   (detections/km^{2})",
                            det_per_dist="D[d]   (detections/km)",
                            det_per_hour="D[t]   (detections/hr)"),
                           default = label_parsed)

# subset for plotting
dfl_dfo = dfl %>% dplyr::filter(box_type == 'DFO')
dfl_tc = dfl %>% dplyr::filter(box_type == 'TC')

# construct labels for each subplot
plot_labs_dfo = dfl_dfo %>% 
  dplyr::filter(!is.infinite(vals)) %>% # remove infinite cost
  group_by(vars) %>% 
  summarize(vals = max(vals, na.rm = TRUE)) %>%
  mutate(label = c('a)', 'b)', 'c)', 'd)', 'e)'),
         n_whales = 20) # set to 0 for left-justified letters
plot_labs_tc = dfl_tc %>% 
  dplyr::filter(!is.infinite(vals)) %>% # remove infinite cost
  group_by(vars) %>% 
  summarize(vals = max(vals, na.rm = TRUE)) %>%
  mutate(label = c('a)', 'b)', 'c)', 'd)', 'e)'),
         n_whales = 20) # set to 0 for left-justified letters

# plot
p = ggplot()+
  geom_path(data = dfl_dfo, aes(x = n_whales, y = vals, group = platform, color = platform))+
  geom_text(data = plot_labs_dfo, aes(x = n_whales, y = vals, label = label)) +
  scale_color_manual(values = platform_cols)+
  labs(x='Number of whales', y='Performance metric value', color = ' Platforms')+
  facet_wrap(~vars, nrow=3, ncol=2, scales = 'free', labeller=my_labeller)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())
p

q = ggplot()+
  geom_path(data = dfl_tc, aes(x = n_whales, y = vals, group = platform, color = platform))+
  geom_text(data = plot_labs_tc, aes(x = n_whales, y = vals, label = label)) +
  scale_color_manual(values = platform_cols)+
  labs(x='Number of whales', y='Performance metric value', color = ' Platforms')+
  facet_wrap(~vars, nrow=3, ncol=2, scales = 'free', labeller=my_labeller)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())
q

# save
ggsave(filename = 'figures/metrics_dfo.png', plot = p, width = 6, height = 8, units = 'in', dpi = 300)
ggsave(filename = 'figures/metrics_tc.png', plot = q, width = 6, height = 8, units = 'in', dpi = 300)

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

# make plots will full data set of whales
dfl_2 = df %>%
  pivot_longer(det_per_hour:cost_per_det, names_to = 'vars', values_to = 'vals') %>%
  dplyr::select(platform, n_whales, box_type, vars, vals)

dfl_2$vars = factor(dfl_2$vars, levels = c("det_per_hour", "det_per_dist", 
                                       "det_per_area", "det_area_time", 
                                       "cost_per_det"), ordered = TRUE)

dfl_2$platform = factor(dfl_2$platform, levels = c("Aircraft","RPAS","Vessel","Slocum glider"), ordered = TRUE)

dfl_dfo_2 = dfl_2 %>% dplyr::filter(box_type == 'DFO')
dfl_tc_2 = dfl_2 %>% dplyr::filter(box_type == 'TC')

plot_labs_dfo_2 = dfl_dfo_2 %>% 
  dplyr::filter(!is.infinite(vals)) %>%
  group_by(vars) %>% 
  summarize(vals = max(vals, na.rm = TRUE)) %>%
  mutate(label = c('a)', 'b)', 'c)', 'd)', 'e)'),
         n_whales = 60)
plot_labs_tc_2 = dfl_tc_2 %>% 
  dplyr::filter(!is.infinite(vals)) %>%
  group_by(vars) %>% 
  summarize(vals = max(vals, na.rm = TRUE)) %>%
  mutate(label = c('a)', 'b)', 'c)', 'd)', 'e)'),
         n_whales = 60)

r = ggplot()+
  geom_path(data = dfl_dfo_2, aes(x = n_whales, y = vals, group = platform, color = platform))+
  geom_text(data = plot_labs_dfo_2, aes(x = n_whales, y = vals, label = label)) +
  scale_color_manual(values = platform_cols)+
  labs(x='Number of whales', y='Performance metric value', color = ' Platforms')+
  facet_wrap(~vars, nrow=3, ncol=2, scales = 'free', labeller=my_labeller)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

s = ggplot()+
  geom_path(data = dfl_tc_2, aes(x = n_whales, y = vals, group = platform, color = platform))+
  geom_text(data = plot_labs_tc_2, aes(x = n_whales, y = vals, label = label)) +
  scale_color_manual(values = platform_cols)+
  labs(x='Number of whales', y='Performance metric value', color = ' Platforms')+
  facet_wrap(~vars, nrow=3, ncol=2, scales = 'free', labeller=my_labeller)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

# save
ggsave(filename = 'figures/_full_metrics_dfo.png', plot = r, width = 6, height = 8, units = 'in', dpi = 300)
ggsave(filename = 'figures/_full_metrics_tc.png', plot = s, width = 6, height = 8, units = 'in', dpi = 300)

