experiment = 1

model_names <- c(
  "full",
  "rate",
  "increase",
  "start",
  "start_increase",
  "start_rate",
  "rate_increase",
  "none"
)


get_loo = function(model, experiment){
  load(here(paste0("output/fe-brms-exp",experiment,"-power-fit-",model,".Rdata")))
  looic = loo(fit)
  looic$estimates["looic","Estimate"]
}
  
model_looic <- unlist(lapply(model_names, function(x) get_loo(x, experiment = experiment)))
names(model_looic) <- model_names
model_looic <- model_looic[order(model_looic)]
save(model_looic, file = here(paste0("output/looic-model-comparison-e",experiment,".Rdata")))
