# Custom made functions

# Extracts sensitivity analyses details for easier handling
get_sens_vals = function(m) {
      if (length(is.na(m)) > 1) { 
      return (c(vcs=vcs, estimate= m$sensitivity_stats[['estimate']], se= m$sensitivity_stats[['se']], t_statistic= m$sensitivity_stats[['t_statistic']], sens_dof= m$sensitivity_stats[['dof']], sens_r2yd_x= m$sensitivity_stats[['r2yd.x']], sens_rv_q= m$sensitivity_stats[['rv_q']][1], sens_rv_qa= m$sensitivity_stats[['rv_qa']][1], sens_f2yd_x= m$sensitivity_stats[['f2yd.x']], bound_r2dz_x= m$bounds[['r2dz.x']], bound_r2yz_dx= m$bounds[['r2yz.dx']], bound_adjusted_estimate= m$bounds[['adjusted_estimate']], bound_adjusted_se= m$bounds[['adjusted_se']], bound_adjusted_t= m$bounds[['adjusted_t']][1], bound_adjusted_lower_ci= m$bounds[['adjusted_lower_CI']], bound_adjusted_upper_ci= m$bounds[['adjusted_upper_CI']]))}}



## Bootstrapping by nesting datasets
meanD = function(data, indices){d <- data[indices]; return(mean(d, na.rm =T))}
boot.summaries = function(boot.fun,metric,y,d) {
  # boot.fun = median or mean function
  # metric = median or mean
  # y = column to summarise
  # d = data frame (groupped)
  d = d %>% nest()
  out = d %>%
  mutate(
        boots = map(data, ~boot(data = .[[y]], statistic = boot.fun, R = 9999)),
        boots.ci = map(boots, boot.ci, type = c("bca")),
        estimate = map(data, ~metric(.[[y]], na.rm =T)),
        conf_low = map(boots.ci, ~.[[4]][4]),
        conf_high = map(boots.ci, ~.[[4]][5])
      ) %>%
  left_join(d %>% summarise(n = map(data, tally))) %>%
  select(-c(data, boots, boots.ci)) %>% unnest(cols = c(estimate, conf_low, conf_high, n)) %>% ungroup()
  return(out)
}
