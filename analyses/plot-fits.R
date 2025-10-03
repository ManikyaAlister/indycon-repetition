library(tidyverse)
library(here)
# 
# exp <- 3
# model <- "increase"
# version <- "power"

plotFit <- function(exp,
                    model = "full",
                    version = "power",
                    title = "Confidence Growth by Consensus Type Across Experiments",
                    subtitle = "Modelled using a power function") {
  d_predictions <- NULL
  d_empirical_summ <- NULL
  
  # load model fit
  load(here(
    paste0(
      "analyses/output/fe-brms-exp",
      exp,
      "-",
      version,
      "-fit-",
      model,
      ".Rdata"
    )
  ))
  
  
  # load and combine empirical data
  load(here(
    paste0("data/experiment-", exp, "/clean/d-modelling.Rdata")
  ))
  
  # define levels
  if (exp %in% 1:2) {
    consensus_levels <- c("dependent", "independent")
    consensus_labels <- c("Dependent", "Independent")
    
  } else if (exp == 3) {
    consensus_levels <- c("dependent", "dependent_source", "independent") # E3 has extra condition
    consensus_labels <- c("Dependent", "Dependent Source", "Independent")
    
  }
  
  # summarise empirical data for plotting
  d_summ_exp <- d_modelling %>%
    mutate(additional_sources = n_sources - 1) %>%
    group_by(consensus, additional_sources) %>%
    summarise(
      mean = mean(confidence),
      sd = sd(confidence),
      n = n()
    ) %>%
    mutate(se = sd / sqrt(n), experiment = exp)
  
  d_empirical_summ <- bind_rows(d_empirical_summ, d_summ_exp)
  
  # Make a prediction grid that spans all values of n_sources for both consensus types
  mean_prior_belief <- mean(d_modelling$prior_belief, na.rm = TRUE)
  
  new_data <- expand.grid(
    n_sources = seq(1, 10, length.out = 50),
    consensus = consensus_levels,
    prior_belief = mean_prior_belief
  ) %>%
    as_tibble()
  
  new_data$consensus <- relevel(new_data$consensus, ref = "independent")
  
  
  # Predict using fixed effects only (no partial pooling)
  d_predictions_exp  <- fitted(fit, newdata = new_data, re_formula = NA) %>%
    as_tibble() %>%
    bind_cols(new_data) %>%
    mutate(additional_sources = n_sources - 1, experiment = exp)
  
  d_predictions <- bind_rows(d_predictions, d_predictions_exp)
  
  plot <- d_predictions %>%
    mutate(consensus = factor(consensus, levels = consensus_levels, labels = consensus_labels)) %>% # match the colour scheme of E1 & E2
    ggplot(aes(x = additional_sources, color = consensus, fill = consensus)) +
    geom_line(size = 1.2, aes(y = Estimate)) +
    geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, y = Estimate),
                alpha = 0.2,
                color = NA) +
    geom_point(data = d_empirical_summ %>%
                 mutate(
                   consensus = factor(consensus, levels = consensus_levels, labels = consensus_labels)
                 ), aes(y = mean)) +
    geom_errorbar(data = d_empirical_summ %>%
                    mutate(
                      consensus = factor(consensus, levels = consensus_levels, labels = consensus_labels)
                    ),
                  aes(ymin = mean - se, ymax = mean + se)) +
    #facet_grid(~ experiment) +
    labs(
      x = "Consensus Proportion",
      y = "Confidence",
      title = title,
      subtitle = subtitle,
      color = "Consensus",
      fill = "Consensus"
    ) +
    # lims(y = c(40,70))+
    scale_fill_viridis_d(option = "D") +
    scale_colour_viridis_d(option = "D") +
    scale_x_continuous(breaks = seq(0, 9, 3),
                       labels = c("1:1", "4:1", "7:1", "10:1")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  
  ggsave(
    plot = plot,
    filename = here(
      paste0("analyses/plots/fe-brms-", version, "-", model, "-fit-E", exp, ".jpg")
    ),
    width = 9,
    height = 5
  )
  
  plot
  
}


