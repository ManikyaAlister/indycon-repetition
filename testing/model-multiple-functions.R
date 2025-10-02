

# rstan::rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())
# 
# # Important: tell Stan to use C++14
# Sys.setenv(LOCAL_CPPFLAGS = "-std=c++14")
rm(list = ls())
library(brms)
library(here)
library(tidyverse)
library(cmdstanr)
experiment = 1

fitModelsConsensusBRMS = function(form_name, data, experiment){
  
  data <- load(here(paste0("data/experiment-",experiment,"/clean/d-modelling.Rdata")))
  
  # Formulas (exponentiated parameters, predictors for consensus)
  forms = list(
    Power = bf(
      confidence ~ (exp(asymptote) + exp(start)) - exp(start) * (n_sources^(-exp(rate))),
      start ~ 1 + consensus,
      asymptote ~ 1 + consensus,
      rate ~ 1 + consensus,
      nl = TRUE
    ),
    
    Exponential = bf(
      confidence ~ exp(asymptote) + exp(start) * (1 - exp(-exp(rate) * n_sources)),
      start ~ 1 + consensus,
      asymptote ~ 1 + consensus,
      rate ~ 1 + consensus,
      nl = TRUE
    ),
    
    Log = bf(
      confidence ~ exp(asymptote) / (1 + exp(start) * exp(-exp(rate) * n_sources)),
      start ~ 1 + consensus,
      asymptote ~ 1 + consensus,
      rate ~ 1 + consensus,
      nl = TRUE
    )
  )
  
  # Priors (baseline + consensus effects)
  priors <- list(
    Power = c(
      prior(normal(log(25), 0.5), nlpar = "start", coef = "Intercept"),
      prior(normal(log(75), 0.5), nlpar = "asymptote", coef = "Intercept"),
      prior(normal(log(0.5), 0.2), nlpar = "rate", coef = "Intercept"),
      prior(normal(0, 0.5), nlpar = "start", class = "b"),
      prior(normal(0, 0.5), nlpar = "asymptote", class = "b"),
      prior(normal(0, 0.2), nlpar = "rate", class = "b")
    ),
    
    Exponential = c(
      prior(normal(log(15), 0.5), nlpar = "start", coef = "Intercept"),
      prior(normal(log(75), 0.5), nlpar = "asymptote", coef = "Intercept"),
      prior(normal(log(0.5), 0.2), nlpar = "rate", coef = "Intercept"),
      prior(normal(0, 0.5), nlpar = "start", class = "b"),
      prior(normal(0, 0.5), nlpar = "asymptote", class = "b"),
      prior(normal(0, 0.2), nlpar = "rate", class = "b")
    ),
    
    Log = c(
      prior(normal(log(0.5), 0.2), nlpar = "start", coef = "Intercept"),
      prior(normal(log(80), 0.5), nlpar = "asymptote", coef = "Intercept"),
      prior(normal(log(0.5), 0.2), nlpar = "rate", coef = "Intercept"),
      prior(normal(0, 0.5), nlpar = "start", class = "b"),
      prior(normal(0, 0.5), nlpar = "asymptote", class = "b"),
      prior(normal(0, 0.2), nlpar = "rate", class = "b")
    )
  )
  
  form <- forms[[form_name]]
  prior <- priors[[form_name]]
  
  # fit one model (no loop)
  fit <- brm(
    formula = form,
    data = d_modelling,
    prior = prior,
    chains = 4, cores = 4, iter = 4000,
    control = list(adapt_delta = 0.95),
    backend = "cmdstanr" 
  )
  
  save(fit, file = here(paste0("analyses/output/brms-exp",experiment,"-",form_name,"-fits-comparing-forms.Rdata")))

  # ---- Predictions ----
  new_data <- expand_grid(
    n_sources = seq(1, 10, length.out = 50),
    consensus = c("dependent", "independent")
  ) %>%
    mutate(consensus = factor(consensus, levels = levels(d_modelling$consensus)))
  
  predictions <- fitted(fit, newdata = new_data, re_formula = NA) %>%
    as_tibble() %>%
    bind_cols(new_data)
  
  # ---- Empirical summary ----
  d_summ <- d_modelling %>%
    group_by(consensus, n_sources) %>%
    summarise(
      mean = mean(confidence, na.rm = TRUE),
      sd   = sd(confidence, na.rm = TRUE),
      n    = n(),
      .groups = "drop"
    ) %>%
    mutate(se = sd/sqrt(n))
  
  # ---- Plot ----
  plot <- ggplot(predictions, aes(x = n_sources, color = factor(consensus), fill = factor(consensus))) +
    geom_line(size = 1.2, aes(y = Estimate)) +
    geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, y = Estimate), alpha = 0.2, color = NA) +
    geom_point(data = d_summ, aes(y = mean)) +
    geom_errorbar(data = d_summ, aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
    labs(
      x = "Number of Additional Sources",
      y = "Predicted Confidence",
      title = paste0(form_name, " Confidence Growth by Consensus Type"),
      subtitle = paste0("Experiment ", experiment),
      color = "Consensus",
      fill  = "Consensus"
    ) +
    theme_minimal(base_size = 14)
  
  ggsave(
    plot = plot,
    filename = here(paste0("plots/brms-exp", experiment, "-", form_name, ".jpg")),
    width = 10, height = 6
  )
  
    fit 
}

all_fit <- NULL

form_name = "Power"
output <- fitModelsConsensusBRMS(form_name, data = d_modelling, experiment = 1)

form_name = "Exponential"
output <- fitModelsConsensusBRMS(form_name, data = d_modelling, experiment = 1)

form_name = "Log"
output <- fitModelsConsensusBRMS(form_name, data = d_modelling, experiment = 1)

form_name = "Power"
output <- fitModelsConsensusBRMS(form_name, data = d_modelling, experiment = 2)


form_name = "Exponential"
output <- fitModelsConsensusBRMS(form_name, data = d_modelling, experiment = 2)

form_name = "Log"
output <- fitModelsConsensusBRMS(form_name, data = d_modelling, experiment = 2)

