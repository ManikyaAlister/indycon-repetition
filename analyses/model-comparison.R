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


getLoo = function(models, experiment){
  model_comparison <- expand_grid(experiment, model) %>%
    mutate(LOOIC = NA, 
           SE = NA)
  
  for (i in 1:length(models)){
    model <- models[i]
    load(here(paste0("analyses/output/fe-brms-exp",experiment,"-power-fit-",model,".Rdata")))
    looic = loo(fit)
    model_comparison[model_comparison[,"model"] == model ,"LOOIC"] <- looic$estimates["looic","Estimate"]
    model_comparison[model_comparison[,"model"] == model ,"SE"] <-looic$estimates["looic","SE"]
  }
  model_comparison
}
  
model_looic <- getLoo(models = model, experiment = experiment)
save(model_looic, file = here(paste0("analyses/output/looic-model-comparison-e",experiment,".Rdata")))