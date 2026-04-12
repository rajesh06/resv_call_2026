avg_case <- extract_anti_diags(med_mal$avg_case)[1:8]
names(avg_case) <- paste0('ye_', 1969:1976)

stan_out <- rstan::stan(file = 
    'C:/Users/rajes/OneDrive/Actuarial Research/resv_call_2026/src/case_adj.stan', 
  model_name = 'berquist sherman case adj', data = list(
    'adj_76_75' = avg_case$ye_1975 / avg_case$ye_1976[1:7], 'N' = 7), 
  par = c('trend', 'adequacy'))
plot_bs(stan_object = stan_out)


