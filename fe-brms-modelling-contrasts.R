rm(list = ls())
library(here)
library(ggpubr)
library(tidyverse)
library(brms)

experiment <- 3

exclude_conditions <- c("dependent_source", "independent") # contrasts should be independent vs. dependent & dependent vs. dependent_source

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



generate_power_priors <- function(data, baseline = "dependent") {
  
  # Ensure consensus is a factor and get its levels
  consensus_levels <- levels(factor(data$consensus))
  consensus_levels <- setdiff(consensus_levels, baseline)  # drop baseline level
  
  # Helper to create prior for one term
  make_prior <- function(nlpar, coef, mean_val, sd_val) {
    eval(bquote(prior(normal(.(mean_val), .(sd_val)), nlpar = .(nlpar), coef = .(coef))))
  }
  
  # Start with intercept priors
  priors <- c(
    make_prior("increase", "Intercept", log(25), 0.25),
    make_prior("start",    "Intercept", log(75), 0.25),
    make_prior("rate",     "Intercept", log(0.5), 0.1)
  )
  
  # Add priors for each non-intercept consensus level
  for (lvl in consensus_levels) {
    coef_name <- paste0("consensus", lvl)
    
    priors <- c(
      priors,
      make_prior("increase", coef_name, 0, 0.25),
      make_prior("start",    coef_name, 0, 0.25),
      make_prior("rate",     coef_name, 0, 0.1)
    )
  }
  
  return(priors)
}



data <- read.csv(here(paste0("data/experiment-",experiment,"/clean/e",experiment,"-long.csv")))

# loop through comparison conditions. 

for (i in 1:length(exclude_conditions)){
  
exclude_cond <- exclude_conditions[i]

# do some cleaning
d_modelling <- data %>%
  mutate(
    prior_belief = scale(views),
    participant = id,
    consensus = relevel(factor(consensus), ref = "independent"),
  ) %>%
  filter(consensus  != exclude_cond)

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
save(fit, file = here(paste0("output/hier-brms-exp",experiment,"-power-fit-excluding-",exclude_cond,".Rdata")))

}
