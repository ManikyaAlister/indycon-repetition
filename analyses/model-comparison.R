experiment = 1

model <- c(
  "none",
  "rate",
  "increase",
  "start",
  "start_increase",
  "start_rate",
  "rate_increase",
  "full"
)


getLoo = function(models, experiment, rm_cond = FALSE){
  model_comparison <- expand_grid(experiment, model) %>%
    mutate(LOOIC = NA, 
           SE = NA)
  
  for (i in 1:length(models)){
    model <- models[i]
    load_script <- paste0("analyses/output/fe-brms-exp",experiment,"-power-fit-",model)
    if(is.character(rm_cond)){
      load_script <- paste0(load_script, "-rm-",rm_cond)
    }
    load(here(paste0(load_script,".Rdata")))
    looic = loo(fit)
    model_comparison[model_comparison[,"model"] == model ,"LOOIC"] <- looic$estimates["looic","Estimate"]
    model_comparison[model_comparison[,"model"] == model ,"SE"] <-looic$estimates["looic","SE"]
  }
  model_comparison
}
  
model_looic <- getLoo(models = model, experiment = experiment)
save(model_looic, file = here(paste0("analyses/output/looic-model-comparison-e",experiment,".Rdata")))

if (experiment == 3) {
  model_looic_rm_independent <- getLoo(models = model, experiment = experiment, rm_cond = "independent")
  save(model_looic_rm_independent, file = here(paste0("analyses/output/looic-model-comparison-e",experiment,"-rm-independent.Rdata")))
  
  model_looic_rm_dependent_source<- getLoo(models = model, experiment = experiment, rm_cond = "dependent_source")
  save(model_looic_rm_dependent_source, file = here(paste0("analyses/output/looic-model-comparison-e",experiment,"-rm-dependent_source.Rdata")))
}


