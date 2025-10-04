library(modelProb)

#define experiment(s)
experiments = 1:3

# models in order of complexity 
models <- c("full", "rate_increase", "start_increase", "start_rate", "increase", "rate", "start", "none")

# define plot lists for ggarrange
weighted_plot_list <- NULL
weighted_stacked_plot_list <- NULL

weighted_group_plot_list <- NULL
weighted_group_stacked_plot_list <- NULL


for (i in 1:length(experiments)) {
  experiment <- experiments[i]
  
  # load model comparison data 
  load(here(paste0("analyses/output/looic-model-comparison-e",experiment,".Rdata")))
  
  d_model_looic <- model_looic %>%
    select(LOOIC)
  
  # convert to to data frame with named column  for weights
  d_model_looic_formatted <- as.data.frame(t(d_model_looic))
  colnames(d_model_looic_formatted) <- model_looic$model

  
  # convert LOOIC to weights
  weighted_LOOIC <- weightedICs( d_model_looic_formatted, bySubject = FALSE)
  
  d_weighted_looic <- as.data.frame(weighted_LOOIC) %>%
    rename(Weight = weighted_LOOIC)
  
  d_weighted_looic$Model = factor(names(weighted_LOOIC), levels = models)
  
  save(d_weighted_looic, file = here(paste0("analyses/output/weighted-looic-model-comparison-e",experiment,".Rdata")))
  
  plotWeights = function(d_weights, experiment, stacked = FALSE){
    
    plot <- d_weights %>%
      ggplot(aes(y = Weight, fill = Model)) + 
      scale_fill_viridis_d() +
      theme_bw() +
      labs(title = paste0("Experiment ",experiment))+
      theme(axis.text.x = element_text(angle = 30, vjust = 0.6))#, hjust=1))
    
    
    if (stacked){
      plot <- plot + geom_bar(stat = "identity", aes(x = ""))
    } else {
      plot <- plot + geom_bar(stat = "identity", aes(x = Model))
    }
    
    plot
    
  }
  
  
  weighted_plot <-  plotWeights(d_weighted_looic, experiment = experiment)
  
  weighted_plot_list[[i]] <- weighted_plot
  
  weight_plot_stacked <- plotWeights(d_weighted_looic, experiment = experiment, stacked = TRUE)
  
  weighted_stacked_plot_list[[i]] <- weight_plot_stacked
  
  # idea: plot "experiment" as participant to see how probability of each model changes/stays consistent across experiments
  
  # grouped model comparison: define the groupings 
  increase_models <- c(models[grepl("increase", models)], "full")
  rate_models <- c(models[grepl("rate", models)], "full")
  start_models <- c(models[grepl("start", models)], "full")
  
  # get combined weights based on groupings 
  looic_increase <- mean(weighted_LOOIC[increase_models])
  looic_rate <- mean(weighted_LOOIC[rate_models])
  looic_start <- mean(weighted_LOOIC[start_models])
  looic_none <- unname(weighted_LOOIC["none"])
  
  
  # re-weight so they sum to 1
  total_sum_looic_weight <- sum(looic_rate, looic_start, looic_increase, looic_none)
  
  looic_group_weights <- c(
    `Increase Models` = looic_increase/total_sum_looic_weight,
    `Start Models` = looic_start/total_sum_looic_weight,
    `Rate Models` =  looic_rate/total_sum_looic_weight,
    None = looic_none/total_sum_looic_weight
  )
  
  # Convert to data frame
  d_looic_groups <- data.frame(
    Model = factor(names(looic_group_weights), levels = names(looic_group_weights)),
    Weight = as.numeric(looic_group_weights)
  )
  
  # Plot stacked barplot
  weighted_group_stacked_plot <- plotWeights(d_looic_groups, experiment = experiment, stacked = TRUE) + labs(x = "Model Grouping")
  weighted_group_stacked_plot_list[[i]] <- weighted_group_stacked_plot

  weighted_group_plot <- plotWeights(d_looic_groups, experiment = experiment) + labs(x = "Model Grouping")
  weighted_group_plot_list[[i]] <- weighted_group_plot + labs(fill = "Model Grouping")
  
  looic_group_weights
  sum(looic_group_weights)
}

experiments_string <- paste(experiments,collapse = "-")

ggarrange(plotlist = weighted_plot_list, common.legend = TRUE, nrow = 1, legend = "none" )

ggarrange(plotlist = weighted_stacked_plot_list, common.legend = TRUE, nrow = 1)

combined_weighted_group_plot <- ggarrange(plotlist = weighted_group_plot_list, common.legend = TRUE, nrow = 1, legend = "none")

save(combined_weighted_group_plot, file = here(paste0("analyses/output/combined-weighted-group-plot-exp-",experiments_string,".Rdata")))
ggsave(filename = here(paste0("analyses/plots/combined-weighted-group-plot-exp-",experiments_string,".png")), width = 9, height = 5)

ggarrange(plotlist = weighted_group_stacked_plot_list, common.legend = TRUE, nrow = 1)


