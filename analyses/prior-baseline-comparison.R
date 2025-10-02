checkPriorBaselineCorr = function(experiment, print_stat = FALSE) {
  load(here(
    paste0("data/experiment-", experiment, "/clean/d-modelling.Rdata")
  ))
  
  # filter so that only baseline trials
  d_baseline <- d_modelling %>%
    filter(n_sources == 1) %>%
    mutate(scale_confidence = as.numeric(scale(confidence)))
  
  corr <- cor.test(d_baseline$prior_belief, d_baseline$scale_confidence)
  r <- paste0("r = ", round(corr$estimate, 2))
  t <- paste0(" t = ", round(corr$statistic, 3))
  p_raw <-  corr$p.value
  if (p_raw < .001) {
    p <- " p < .001"
  } else {
    p <- paste0(" p = ", round(p_raw, 3))
  }
  
  if(print_stat){
    print(corr)
  }
  
  d_baseline %>%
    ggplot(aes(x = prior_belief, y = scale_confidence)) +
    geom_point() +
    labs(x = "Prior Belief",
         y = "Confidence at Baseline",
         subtitle = paste(r, p, t, sep = ","),
         title = paste0("Experiment ", experiment)) +
    theme_bw()
  
}

checkPriorBaselineGroups = function(experiment,print_stat = FALSE) {
  load(here(
    paste0("data/experiment-", experiment, "/clean/d-modelling.Rdata")
  ))
  
  
  # filter so that only basline trials
  d_baseline <- d_modelling %>%
    filter(n_sources == 1) %>%
    mutate(scale_confidence = as.numeric(scale(confidence)))
  
  # compare groups time points using paired t test
  ttest <- t.test(d_baseline$prior_belief,
                  d_baseline$scale_confidence,
                  paired = TRUE)
  if(print_stat){
    print(ttest)
  }
 
  
  # extract relevant stats
  t <- paste0(" t(", ttest$parameter, ") = ", round(ttest$statistic, 3))
  p_raw <-  ttest$p.value
  if (p_raw < .001) {
    p <- " p < .001"
  } else {
    p <- paste0(" p = ", round(p_raw, 3))
  }
  
  subtitle = paste(t, p, sep = ",")
  title = paste0("Experimemnt ", experiment)
  
  # arrange data in a way that is appropriate for the comoparison raincloud plot
  d_raincloud <- data_1x1(
    array_1 = d_baseline$prior_belief,
    array_2 = d_baseline$scale_confidence,
    jit_distance = .09,
    jit_seed = 321
  )
  
  # Step 2: Create the raincloud plot with linked repeated measures
  raincloud_prior_vs_confidence <- raincloud_1x1_repmes(
    data = d_raincloud,
    colors = (c('dodgerblue', 'darkorange')),
    fills = (c('dodgerblue', 'darkorange')),
    line_color = 'gray',
    line_alpha = .3,
    size = 1,
    alpha = .6,
    align_clouds = FALSE
  ) +
    scale_x_continuous(
      breaks = c(1, 2),
      labels = c("Prior Belief", "Confidence at Baseline"),
      limits = c(0, 3)
    ) +
    labs(x = "Measure", y = "Scaled Value", subtitle = subtitle, title = title) +
    theme_bw()
  
  # Display the plot
  raincloud_prior_vs_confidence
  
}
