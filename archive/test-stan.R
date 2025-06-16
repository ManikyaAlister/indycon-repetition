#rm(list = ls())
library(here)
library(tidyverse)
library(rstan)
experiment = 2
i = 2
load(here(paste0("data/experiment-",experiment,"/clean/d-n-repetitions.Rdata")))

d_ind <- data %>%
  filter(consensus == "dependent")

output <- nonLinearRegression(form = forms[[1]], d_model = d_ind)
summ_output <- summary(output)
# Plotting ----------------------------------------------------------------

n_sources <- 0:11
params <-  summ_output$summary


sim_data <- data.frame(
  model_predictions = simulateExp(params, "50%", n_sources),
  upper_CI = simulateExp(params, "97.5%", n_sources),
  lower_CI = simulateExp(params, "2.5%", n_sources),
  n_sources = n_sources,
  consensus = cond,
  experiment = i
)

# load real data
load(here(paste0("data/experiment-",i,"/clean/d-n-repetitions.Rdata")))

# summarise by repetition
d_summ <- data %>%
  group_by(consensus, n_sources) %>%
  summarise(median = median(confidence), sd = sd(confidence), n = n())%>%
  mutate(se = sd/sqrt(n),
         experiment = i)

sim_data %>% 
  ggplot(aes(
    x = n_sources,
    y = model_predictions,
    group = consensus,
    colour = consensus
  )) +
  geom_line() +
  geom_point(data = d_summ, aes(y = median))+
  #geom_jitter(data = d_ind, aes(y = confidence))+
  facet_wrap(~experiment)
