library(here)
library(tidyverse)
library(brms)

experiment = 2
no_exclusions_str <- "-no-exclusions" # "-no-exclusions" or NULL 
re = FALSE #re_valence # re = random effects model

model <- c(
  "none",
  "rate",
  "increase",
  "start",
  "start_increase",
  "start_rate",
  "rate_increase",
  "full"
  #"re_claim"
)

output_dir <- paste0("analyses/output/looic-model-comparison-e",experiment)


if (is.character(re)){
  model <-  paste0(re,"_", model)  # random effect on claim for experiment 3
  output_dir <- paste0(output_dir, "-",re)
}


getLoo = function(models, experiment, rm_cond = FALSE, no_exclusions_str = NULL){
  
  loo_list <- list()
  model_comparison <- expand_grid(experiment, model) %>%
    mutate(LOOIC = NA, 
           SE = NA)
  
  for (i in 1:length(models)){
    model <- models[i]
    print(paste0("Model: ", model))
    load_script <- paste0("analyses/output/fe-brms-exp-",experiment,"-power-fit-",model)
    if(is.character(rm_cond)){
      load_script <- paste0(load_script, "-rm-",rm_cond)
    }
    load(here(paste0(load_script,no_exclusions_str,".Rdata")))
    looic <- loo(fit)
    loo_list[[model]] <- looic
    model_comparison[model_comparison[,"model"] == model ,"LOOIC"] <- looic$estimates["looic","Estimate"]
    model_comparison[model_comparison[,"model"] == model ,"SE"] <-looic$estimates["looic","SE"]
  }
  
  formal_loo_comparison <- loo_compare(loo_list)
  
  list(
    formal_loo_comparison,
    model_comparison,
    loo_list
  )
  
}

  
model_looic <- getLoo(models = model, experiment = experiment, no_exclusions_str = no_exclusions_str)

save(model_looic, file = here(paste0(output_dir,no_exclusions_str,".Rdata")))

if (experiment == 2) {
  rm_dep_fil <- "looic"
  
  model_looic_rm_independent <- getLoo(models = model, experiment = experiment, rm_cond = "independent", no_exclusions_str = no_exclusions_str)
  
  path_rm_ind <- paste0(output_dir,"-rm-independent")
  save(model_looic_rm_independent, file = here(paste0(path_rm_ind,no_exclusions_str,".rdata")))
  
  model_looic_rm_dependent_source<- getLoo(models = model, experiment = experiment, rm_cond = "dependent_source", no_exclusions_str = no_exclusions_str)
  
  path_rm_dep <- paste0(output_dir,"-rm-dependent_source")
  save(model_looic_rm_dependent_source, file = here(paste0(path_rm_dep,no_exclusions_str,".rdata")))
}


