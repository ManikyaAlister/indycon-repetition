library(tidyverse)
library(posterior)

experiment = 1

# load model output
load(here(paste0("output/fe-brms-exp",experiment,"-power-fit.Rdata")))

# Evaluate fit ------------------------------------------------------------

# Make a prediction grid that spans all values of n_sources for both consensus types
new_data <- expand.grid(
  n_sources = seq(1, 10, length.out = 50),
  consensus = c("dependent", "independent")
) %>%
  as_tibble()

# Predict using fixed effects only (no partial pooling)
predictions <- fitted(fit, newdata = new_data, re_formula = NA) %>%
  as_tibble() %>%
  bind_cols(new_data) %>%
  mutate(
    additional_sources = n_sources - 1
  )


# summarise real data for plotting
d_summ <- d_modelling %>%
  mutate(
    additional_sources = n_sources - 1
  ) %>%
  group_by(consensus, additional_sources) %>%
  summarise(mean = mean(confidence), sd = sd(confidence), n = n())%>%
  mutate(se = sd/sqrt(n))


plot <- ggplot(predictions, aes(x = additional_sources, color = consensus, fill = consensus)) +
  geom_line(size = 1.2, aes(y = Estimate)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, y = Estimate), alpha = 0.2, color = NA) +
  geom_point(data = d_summ, aes(y = mean))+
  geom_errorbar(data = d_summ, aes(ymin = mean - se, ymax = mean + se))+
  labs(
    x = "Number of Additional Sources (Consensus Size)",
    y = "Predicted Confidence",
    title = paste0("Power"," Confidence Growth by Consensus Type"),
    subtitle = paste0("Experiment ", experiment),
    color = "Consensus",
    fill = "Consensus"
  ) +
  scale_fill_viridis_d(option = "D")+
  scale_colour_viridis_d(option = "D")+
  scale_x_continuous(breaks = seq(0,9,3))+
  theme_minimal(base_size = 14)

plot

ggsave(plot = plot, filename = here(paste0("plots/fe-brms-exp",experiment,"-power-fit.jpg")), width = 7, height = 4)



# Compare parameters across conditions ------------------------------------

# Extract draws
draws <- as_draws_df(fit)

# 2. Helper to extract and transform parameters
get_param_contrasts <- function(draws, param_name) {
  intercept <- draws[[paste0("b_", param_name, "_Intercept")]]
  condition_effect <- draws[[paste0("b_", param_name, "_consensusindependent")]]
  
  dep <- exp(intercept)
  indep <- exp(intercept + condition_effect)
  
  tibble(
    parameter = param_name,
    dep = dep,
    indep = indep,
    abs_diff = indep - dep,
    rel_diff = (indep - dep) / dep
  )
}

# 3. Individual parameter contrasts
log_params <- c("rate", "increase", "start")
param_contrasts <- bind_rows(lapply(log_params, function(p) get_param_contrasts(draws, p)))

# 4. Derived "true" asymptote = start + increase
asymptote_dep <- exp(draws$b_start_Intercept) + exp(draws$b_increase_Intercept)
asymptote_indep <- exp(draws$b_start_Intercept + draws$b_start_consensusindependent) +
  exp(draws$b_increase_Intercept + draws$b_increase_consensusindependent)

param_contrasts <- bind_rows(
  param_contrasts,
  tibble(
    parameter = "asymptote",
    dep = asymptote_dep,
    indep = asymptote_indep,
    abs_diff = asymptote_indep - asymptote_dep,
    rel_diff = (asymptote_indep - asymptote_dep) / asymptote_dep
  )
)

# 5. Summarise posterior contrasts
param_contrasts_summary <- param_contrasts %>%
  group_by(parameter) %>%
  summarise(
    mean_dep = mean(dep),
    mean_indep = mean(indep),
    mean_abs_diff = mean(abs_diff),
    ci_abs_lower = quantile(abs_diff, 0.025),
    ci_abs_upper = quantile(abs_diff, 0.975),
    prob_indep_gt_dep = mean(abs_diff > 0),
    
    mean_rel_diff = mean(rel_diff),
    ci_rel_lower = quantile(rel_diff, 0.025),
    ci_rel_upper = quantile(rel_diff, 0.975)
  ) %>%
  arrange(match(parameter, c("rate", "increase", "start", "asymptote")))

param_contrasts_long <- param_contrasts %>%
  pivot_longer(
    cols = c(dep, indep),
    names_to = "consensus",
    values_to = "value"
  )


# Plot

param_contrasts_long %>%
  filter(parameter != "increase") %>% # remove increase parameter because it is basically the same as asymptote
ggplot(aes(x = value, colour = consensus, fill = consensus)) +
  stat_halfeye(alpha = 0.6, slab_linewidth = 0.5, slab_color = "black",  normalize = "xy", .width = .95) +
  facet_wrap(~parameter, scales = "free_x")+
  #facet_grid(form + experiment ~ parameter , scales = "free") +
  theme_minimal() +
  labs(
    title = "Posterior Distributions by Consensus Type and Experiment",
    subtitle = paste0("Experiment ", experiment),
    x = "Estimate",
    y = NULL,
    fill = "Consensus",
    color = "Consensus"
  )

