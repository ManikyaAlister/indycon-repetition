rm(list = ls())
library(here)
library(ggpubr)
library(tidyverse)
library(brms)

# source models defined in models.R
source("analyses/models.R")

# source priors 
source("analyses/priors.R")

experiment <- 2 #"1-combined"
rm_cond <- "dependent_source" # set to FALSE if don't want to remove any conditions, otherwise for experiment 2 rm either "dependent_source" or "independent"
exclude_ps <- FALSE # Run with exclusions (TRUE) or without (FALSE)
re_valence <- FALSE # Run valence as a random effect (TRUE)
re_claim <- FALSE  # Run claim as a random effect (TRUE)

model_names <- c("full", "rate",  "increase", "start", "start_increase", "start_rate","rate_increase", "none")
 
if (experiment == 2 & re_claim){
  model_names <-  paste0("re_claim_", model_names)  # random effect on claim for experiment 3
} else if (experiment == "1-combined" & re_valence) {
  model_names <- paste0("re_valence_", model_names) # random effect on valence for e1 combined
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
  
  file_d <- paste0(
    "data/experiment-",experiment,"/clean/d-modelling"
  )
  
  if (exclude_ps){
  d_modelling <- d_modelling %>%
    filter(final_inclusion == 1)
  } else {
    file_d <- paste0(file_d, "-no-exclusions")
  }
  
  save(d_modelling, file = here(paste0(file_d,".Rdata")))
  
  if(is.character(rm_cond)){
    d_modelling <- d_modelling %>%
      filter(consensus != rm_cond)
  }
  

  power_priors <- generate_power_priors(d_modelling)
  
  fitHierModel = function(data, form, prior, experiment, form_name){
    
    # Fit the model 
    fit <- brm(
      formula = form,
      data = data,
      family = gaussian(),
      prior = prior,
      chains = 4,
      cores = 4#,
      #backend = "cmdstanr"  
    )
    
    fit
    
  }
  
  fit <- fitHierModel(d_modelling, power, power_priors, experiment, "Power")
  
  file <- paste0("analyses/output/fe-brms-exp-",experiment,"-power-fit-",model_name)
  
  if(is.character(rm_cond)){
    file <- paste0(file,"-rm-",rm_cond)
  }
  
  if (!exclude_ps) {
   file <- paste0(file, "-no-exclusions") 
  }
  
  save(fit, file = here(paste0(file,".Rdata")))
}




