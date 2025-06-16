# rstan::rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())
# 
# # Important: tell Stan to use C++14
# Sys.setenv(LOCAL_CPPFLAGS = "-std=c++14")
rm(list = ls())
library(brms)
library(here)
library(tidyverse)
library(cmdstanr)
experiment = 2


fitModelsConsensusBRMS = function(form_name, variable = "consensus", data = d_modelling, experiment){
  
  data <- read.csv(here(paste0("data/experiment-",experiment,"/clean/con_rep_Exp",experiment,"_cogsci.csv")))
  
  # do some cleaning
  d_modelling <- data %>%
    mutate(
      n_sources = case_when(
        repetitions == "Baseline" ~ 1,
        repetitions == "Phase 1" ~ 4,
        repetitions == "Phase 2" ~ 7,
        repetitions == "Phase 3" ~ 10
      ),
      views_normalized = (data$views / 7) * 100,
    )
   
  # Define the nonlinear formula
  forms = list(
    Exponential = bf(
      confidence ~   (asymptote + start) + start * (1 - exp(-rate * n_sources)),
      asymptote + rate + start ~ 1,
      nl = TRUE
    ),
    
    Power = bf(
      confidence ~ (asymptote + start) - start * (n_sources^-rate),
      asymptote + rate + start ~ 1,
      nl = TRUE
    ), 
    Log  = bf(
      confidence ~ asymptote/(1 + start * exp(-rate*n_sources)),
      asymptote + rate + start ~ 1,
      nl = TRUE
    ) 
  )
  
  # Define priors
  priors <- list(
    Power = c(
      prior(normal(50, 10), nlpar = "start"),
      prior(normal(75, 10), nlpar = "asymptote"),
      prior(normal(0.5, 0.2), nlpar = "rate", lb = 0)
    ),
    
    Exponential = c(
      prior(normal(15, 10), nlpar = "start"),
      prior(normal(75, 10), nlpar = "asymptote"),
      prior(normal(0.5, 0.2), nlpar = "rate", lb = 0)
    ),
    
    Log = c(
      prior(normal(0.5, 0.2), nlpar = "start", lb = 0),
      prior(normal(80, 10), nlpar = "asymptote", lb = 0, ub = 100),
      prior(normal(0.5, 0.2), nlpar = "rate", lb = 0)
    )
  )
  
  form <- forms[[form_name]]
  prior <- priors[[form_name]] 
  
  
  # identify each of the variable levels you want to compare 
  variable_levels <- unique(data[,variable])
  n_levels <- length(variable_levels)
  
  # print so we can see in the console and make sure we're running on the correct conditions
  print(variable_levels)
  
  # define some empty objects for output we want to save
  predictions = NULL 
  fits = list()
  
  # loop through each condition we want to compare
  for (i in 1:n_levels){
    
    # define the condition for this iteration
    condition_i <- variable_levels[i]
    
    # define data for this iteration
    d_i = filter(d_modelling, consensus == condition_i)

    # Fit the model for the condition of interest 
    fit_i <- brm(
      formula = form,
      data = d_i,
      family = gaussian(),
      prior = prior,
      chains = 4,
      cores = 4,
      backend = "cmdstanr"  
    )
    
    # check and save convergence plots
    converge <- plot(fit_i)
    converge_combined = ggarrange(plotlist = converge) # technically a list so need to turn it into a ggarrange 
    ggsave(plot = converge_combined, filename = here(paste0("plots/brms-exp",experiment,"-",form_name,"-",condition_i,"-convergence.jpg")), width = 6, height = 10)
    
    
    # Make a new data frame for prediction
    new_data <- tibble(
      n_sources = seq(1, 10, length.out = 50)
    )
    
    # Get fitted predictions
    pred_i <- fitted(fit_i, newdata = new_data, re_formula = NA) %>%
      as_tibble() %>%
      bind_cols(new_data) %>%
      mutate("{variable}" := condition_i)
   
    # add fits and predictions to single list/data frame
    predictions <- bind_rows(predictions, pred_i)
    fits[[i]] <- fit_i
  }
  
  names(fits) <- variable_levels

  save(fits, file = here(paste0("output/brms-exp",experiment,"-",form_name,"fits.Rdata")))
  
  
  
  # summarise real data for plotting
  d_summ <- d_modelling %>%
    group_by(consensus, n_sources) %>%
    summarise(mean = mean(confidence), sd = sd(confidence), n = n())%>%
    mutate(se = sd/sqrt(n))
  
  
  plot <- ggplot(predictions, aes(x = n_sources, color = consensus, fill = consensus)) +
    geom_line(size = 1.2, aes(y = Estimate)) +
    geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, y = Estimate), alpha = 0.2, color = NA) +
    geom_point(data = d_summ, aes(y = mean))+
    geom_errorbar(data = d_summ, aes(ymin = mean - se, ymax = mean + se))+
    labs(
      x = "Number of Additional Sources",
      y = "Predicted Confidence",
      title = paste0(form_name," Confidence Growth by Consensus Type"),
      subtitle = paste0("Ecperiment ", experiment),
      color = "Consensus",
      fill = "Consensus"
    ) +
    theme_minimal(base_size = 14)
  
  ggsave(plot = plot, filename = here(paste0("plots/brms-exp",experiment,"-",form_name,".jpg")), width = 10, height = 6)
  
  
  list(fits = fits,
       predictions = predictions)
}

form_name = "Power"
output <- fitModelsConsensusBRMS(form_name, data = d_modelling, experiment = 1)


form_name = "Exponential"
output <- fitModelsConsensusBRMS(form_name, data = d_modelling, experiment = 1)

form_name = "Log"
output <- fitModelsConsensusBRMS(form_name, data = d_modelling, experiment = 1)


form_name = "Power"
output <- fitModelsConsensusBRMS(form_name, data = d_modelling, experiment = 2)


form_name = "Exponential"
output <- fitModelsConsensusBRMS(form_name, data = d_modelling, experiment = 2)

form_name = "Log"
output <- fitModelsConsensusBRMS(form_name, data = d_modelling, experiment = 2)

