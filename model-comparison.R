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


get_loo = function(model){
  load(here(paste0("output/fe-brms-exp3-power-fit-",model,".Rdata")))
  looic = loo(fit)
  looic$estimates["looic","Estimate"]
}

model_looic <- unlist(lapply(model_names, get_loo))
names(model_looic) <- model_names
model_looic <- model_looic[order(model_looic)]
save(model_looic, file = here("output/looic-model-comparison.Rdata"))