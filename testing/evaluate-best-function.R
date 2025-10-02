rm(list = ls())
library(tidyverse)
library(here)
library(brms)

experiment <- 1:2
forms <- c("Power", "Exponential", "Log")
form_comparison <- expand_grid(experiment, forms) %>%
  mutate(LOOIC = NA, 
         SE = NA)

for (exp in experiment){
  for (i in 1:length(forms)){
    form <- forms[i]
    load(here(paste0("analyses/output/brms-exp",exp,"-",form,"-fits-comparing-forms.Rdata")))
    
    looic = loo(fit)
    form_comparison[form_comparison[,"experiment"] == exp & form_comparison[,"forms"] == form ,"LOOIC"] <- looic$estimates["looic","Estimate"]
    form_comparison[form_comparison[,"experiment"] == exp & form_comparison[,"forms"] == form ,"SE"] <-looic$estimates["looic","SE"]
  }
}

save(form_comparison, file = here("analyses/output/form_comparison_looc.Rdata"))

# check model rank in each experiment according to BIC
form_comparison %>%
  filter(experiment == 1) %>%
  mutate(rank = rank(LOOIC))

