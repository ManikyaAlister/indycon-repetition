experiment = 2
re_claim = FALSE

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


if (re_claim){
  model <-  paste0("re_claim_", model)  # random effect on claim for experiment 3
  output_dir <- paste0(output_dir, "-re_claim")
  
}


getLoo = function(models, experiment, rm_cond = FALSE){
  
  loo_list <- list()
  model_comparison <- expand_grid(experiment, model) %>%
    mutate(LOOIC = NA, 
           SE = NA)
  
  for (i in 1:length(models)){
    model <- models[i]
    print(paste0("Model: ", model))
    load_script <- paste0("analyses/output/fe-brms-exp",experiment,"-power-fit-",model)
    if(is.character(rm_cond)){
      load_script <- paste0(load_script, "-rm-",rm_cond)
    }
    load(here(paste0(load_script,".Rdata")))
    looic <- loo(fit)
    loo_list[[model]] <- looic
    model_comparison[model_comparison[,"model"] == model ,"LOOIC"] <- looic$estimates["looic","Estimate"]
    model_comparison[model_comparison[,"model"] == model ,"SE"] <-looic$estimates["looic","SE"]
  }
  
  formal_loo_comparison <- loo_compare(loo_list)
  
  list(
    formal_loo_comparison,
    model_comparison
  )
  
}

  
model_looic <- getLoo(models = model, experiment = experiment)

save(model_looic, file = here(paste0(output_dir,".Rdata")))

if (experiment == 3) {
  rm_dep_fil <- "looic"
  
  model_looic_rm_independent <- getLoo(models = model, experiment = experiment, rm_cond = "independent")
  
  path_rm_ind <- paste0(output_dir,"-rm-independent")
  save(model_looic_rm_independent, file = here(paste0(path_rm_ind,".rdata")))
  
  model_looic_rm_dependent_source<- getLoo(models = model, experiment = experiment, rm_cond = "dependent_source")
  
  path_rm_dep <- paste0(output_dir,"-rm-dependent_source")
  save(model_looic_rm_dependent_source, file = here(paste0(path_rm_dep,".rdata")))
}


