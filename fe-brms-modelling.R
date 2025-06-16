rm(list = ls())
library(here)
library(ggpubr)
library(tidyverse)
library(brms)

experiment = 2

# define log model
power <- bf(
  # I need to make sure that the parameters can never be < 0. Conventionally, 
  # you would do this by setting a lower bound in the priors, but this will also 
  # restrict the difference between independent and dependent to be >0 for all 
  # parameters, which I do not want, so that I can identify cases where there are no real differences. 
  # You still need to set some kind of restriction, though, because otherwise the model will have trouble converging. 
  # My solution is to exponentiation each parameter, meaning that they are functionally constrained to be positive. 
  confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
  start ~ 1 + consensus + prior_belief,
  increase ~ 1 + consensus + prior_belief,
  rate ~ 1 + consensus + prior_belief,
  nl = TRUE
  
)


power_priors <- c(
  
  prior(normal(log(25), 0.5), nlpar = "increase", coef = "Intercept"),  # apply log to the prior to account for the exponentiation. 
  prior(normal(0, 0.5), nlpar = "increase", coef = "consensusindependent"),
  #prior(normal(0, 0.5), nlpar = "increase", coef = "prior_belief"),
  
  
  prior(normal(log(75), 0.5), nlpar = "start", coef = "Intercept"),
  prior(normal(0, 0.5), nlpar = "start", coef = "consensusindependent"),
  #prior(normal(0, 0.2), nlpar = "start", coef = "prior_belief"),
  
  prior(normal(log(0.5), 0.2), nlpar = "rate", coef = "Intercept"),
  prior(normal(0, 0.2), nlpar = "rate", coef = "consensusindependent")
  #prior(normal(0, 0.2), nlpar = "rate", coef = "prior_belief")

)


data <- read.csv(here(paste0("data/experiment-",experiment,"/clean/e",experiment,"-long.csv")))

# do some cleaning
d_modelling <- data %>%
  mutate(
    n_sources = case_when(
      repetitions == "Baseline" ~ 1,
      repetitions == "Phase 1" ~ 4,
      repetitions == "Phase 2" ~ 7,
      repetitions == "Phase 3" ~ 10
    ), 
    prior_belief = scale(views),
    participant = id
  )

fitHierModel = function(data, form, prior, experiment, form_name){
  
  # Fit the model 
  fit <- brm(
    formula = form,
    data = d_modelling,
    family = gaussian(),
    prior = prior,
    chains = 4,
    cores = 4,
    backend = "cmdstanr"  
  )
  
  fit
  
}

fit <- fitHierModel(d_modelling, power, power_priors, experiment, "Power")
save(fit, file = here(paste0("output/hier-brms-exp",experiment,"-power-fit.Rdata")))


