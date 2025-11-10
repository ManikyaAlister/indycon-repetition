rm(list = ls())
library(here)
library(ggpubr)
library(tidyverse)
library(brms)

# aource models defined in models.R
source("analyses/models.R")

experiment <- 3
rm_cond <- "dependent_source" # set to FALSE if don't want to remove any conditions
#re_claim <- FALSE # whether we want a random effect on claim

model_names <- "full" #c( "rate",  "increase", "start", "start_increase", "start_rate","rate_increase", "none") #, full_hierarchical")#,
 
if (experiment == 3){
  #re_claim <- TRUE 
  model_names <-  paste0("re_claim_", model_names)  # random effect on claim for experiment 3
}

for(j in 1:length(model_names)){
  model_name <- model_names[j]
  
  print(paste0("Model: ", model_name))
  
  # define log model
  power <- models[[model_name]]
  
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
  
  if(is.character(rm_cond)){
    d_modelling <- d_modelling %>%
      filter(consensus != rm_cond)
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
  
  file <- paste0("analyses/output/fe-brms-exp",experiment,"-power-fit-",model_name)
  
  if(is.character(rm_cond)){
    file <- paste0(file,"-rm-",rm_cond)
  }
  
  save(fit, file = here(paste0(file,".Rdata")))
}




