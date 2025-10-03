rm(list = ls())
library(here)
library(ggpubr)
library(tidyverse)
library(brms)

experiment = 2
model_names <- "none" #c("rate",  "increase", "start", "start_increase", "start_rate","rate_increase", "none")# "re_claim", full_hierarchical")#,
for(j in 1:length(model_names)){
  model_name <- model_names[j]
  models <- list(
    "full" =  bf(
      # I need to make surethat the parameters can never be < 0. Conventionally, 
      # you would do this by setting a lower bound in the priors, but this will also 
      # restrict the difference between independent and dependent to be >0 for all 
      # parameters, which I do not want, so that I can identify cases where there are no real differences. 
      # You still need to set some kind of restriction, though, because otherwise the model will have trouble converging. 
      # My solution is to exponentiation each parameter, meaning that they are functionally constrained to be positive. 
      confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
      start ~ 1 + consensus,# + prior_belief,
      increase ~ 1 + consensus, #+ prior_belief,
      rate ~ 1 + consensus,#+ prior_belief,
      nl = TRUE
      
    ),
    # remove asymptote (increase parameter) varying by consensus
    "start_rate" =  bf( 
      confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
      start ~ 1 + consensus,# + prior_belief,
      increase ~ 1, #+ consensus, # + prior_belief,
      rate ~ 1 + consensus, #+ prior_belief,
      nl = TRUE
    ),
    "start_increase" =  bf( 
      confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
      start ~ 1 + consensus,
      increase ~ 1 + consensus,
      rate ~ 1,
      nl = TRUE
    ),
    "rate_increase" =  bf( 
      confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
      start ~ 1,
      increase ~ 1 + consensus,
      rate ~ 1 + consensus,
      nl = TRUE
    ),
    "rate" =  bf( 
      confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
      start ~ 1,
      increase ~ 1, 
      rate ~ 1 + consensus, 
      nl = TRUE
    ),
    "start" =  bf( 
      confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
      start ~ 1 + consensus,
      increase ~ 1, 
      rate ~ 1,
      nl = TRUE
    ),
    "increase" =  bf( 
      confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
      start ~ 1,
      increase ~ 1 + consensus, 
      rate ~ 1, 
      nl = TRUE
    ),
    "none" =  bf( 
      confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
      start ~ 1,
      increase ~ 1, 
      rate ~ 1, 
      nl = TRUE
    ),
    "full_hierarchical" = bf(
      confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
      # Each parameter varies by consensus (fixed effect) 
      # and by claim (hierarchical random effect)
      start    ~ 1 + consensus + (1 + consensus | claim),
      increase ~ 1 + consensus + (1 + consensus | claim),
      rate     ~ 1 + consensus + (1 + consensus | claim),
      nl = TRUE
    ),
    "re_claim" = bf(
      confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
      # Each parameter varies by consensus (fixed effect) 
      # and by claim (hierarchical random effect)
      start    ~ 1 + consensus + (1 | claim),
      increase ~ 1 + consensus + (1 | claim),
      rate     ~ 1 + consensus + (1 | claim),
      nl = TRUE
    )
    
  )
  # define log model
  power <- models[[model_name]]
  
  generate_power_priors <- function(data, baseline = "dependent", model = model_name) {
    
    # Ensure consensus is a factor and get its levels
    consensus_levels <- levels(factor(data$consensus))
    consensus_levels <- setdiff(consensus_levels, baseline)  # drop baseline level
    
    # Helper to create prior for one term
    make_prior <- function(nlpar, coef, mean_val, sd_val) {
      eval(bquote(prior(normal(.(mean_val), .(sd_val)), nlpar = .(nlpar), coef = .(coef))))
    }
    
    # Start with intercept priors
    priors <- c(
      make_prior("increase", "Intercept", log(25), 0.5),
      make_prior("start",    "Intercept", log(75), 0.5),
      make_prior("rate",     "Intercept", log(0.5), 0.2)
    )
    
    # Add priors for each non-intercept consensus level
    for (lvl in consensus_levels) {
      coef_name <- paste0("consensus", lvl)
      
      if (grepl("asym", model)| grepl("full", model)){
        priors <- c(
          priors,
          make_prior("increase", coef_name, 0, 0.5))
      }
      
      if (grepl("start", model)| grepl("full", model)){
        priors <- c(
          priors,
          make_prior("start",    coef_name, 0, 0.5)
          )
      }
      
      if (grepl("rate", model)| grepl("full", model)){
        priors <- c(
          priors,
          make_prior("rate",    coef_name, 0, 0.5))
      }
      
    }
    
    return(priors)
  }
  
  
  
  
  data <- read.csv(here(paste0("data/experiment-",experiment,"/clean/e",experiment,"-long.csv")))
  
  # do some cleaning
  d_modelling <- data %>%
    mutate(
      prior_belief = as.numeric(scale(views)),
      participant = id,
      consensus = relevel(factor(consensus), ref = "dependent"),
    )
  
  if (experiment %in% 1:2){ # already done in E3
    d_modelling <- d_modelling %>%
      mutate(
        n_sources = case_when(
          repetitions == "Baseline" ~ 1,
          repetitions == "Phase 1" ~ 4,
          repetitions == "Phase 2" ~ 7,
          repetitions == "Phase 3" ~ 10
        ))
  }
  
  save(d_modelling, file = here(paste0(
    "data/experiment-",experiment,"/clean/d-modelling.Rdata"
  )))
  power_priors <- generate_power_priors(d_modelling)
  
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
  save(fit, file = here(paste0("analyses/output/fe-brms-exp",experiment,"-power-fit-",model_name,".Rdata")))
}




