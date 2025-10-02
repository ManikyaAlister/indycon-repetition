library(tidyverse)
library(brms)
library(here)

# fits is your list of brms fits; experiments is your loop variable
d_predictions <- NULL
d_empirical_summ <- NULL

experiments <- 3 # only hierarchical for e3 because multiple claims 

for (exp in experiments) {
  # load empirical data used for that experiment (modify path if needed)
  load(here(paste0("data/experiment-", exp, "/clean/d-modelling.Rdata")))
  
  # load fits 
  load(here(paste0("analyses/output/fe-brms-exp3-power-fit-full_hierarchical.Rdata")))
  
  
  # summarize empirical data (for plotting)
  d_summ_exp <- d_modelling %>%
    mutate(additional_sources = n_sources - 1) %>%
    group_by(consensus, additional_sources) %>%
    summarise(mean = mean(confidence), sd = sd(confidence), n = n(), .groups = "drop") %>%
    mutate(se = sd / sqrt(n), experiment = exp)
  
  d_empirical_summ <- bind_rows(d_empirical_summ, d_summ_exp)
  
  # prediction grid (includes claim for claim-level predictions)
  mean_prior_belief <- mean(d_modelling$prior_belief, na.rm = TRUE)
  claims <- unique(d_modelling$claim)   # ensure claim column exists
  
  newdata_pop <- expand.grid(
    n_sources = seq(1, 10, length.out = 50),
    consensus = c("dependent", "independent", "dependent_source"),
    prior_belief = mean_prior_belief
  ) %>% as_tibble() %>%
    mutate(consensus = factor(consensus, levels = c("dependent", "dependent_source", "independent")),
           experiment = exp,
           additional_sources = n_sources - 1)
  
  # population-level (no group effects)
  pop_pred <- fitted(fit, newdata = newdata_pop, re_formula = NA, probs = c(0.025, 0.975)) %>%
    as_tibble() %>%
    bind_cols(newdata_pop)
  
  # claim-level predictions: include each claim in the grid so we can plot per-claim curves
  # (if you have many claims you may sample a subset; here we use all)
  newdata_claim <- expand.grid(
    n_sources = seq(1, 10, length.out = 50),
    consensus = c("dependent", "independent", "dependent_source"),
    prior_belief = mean_prior_belief,
    claim = claims
  ) %>% as_tibble() %>%
    mutate(consensus = factor(consensus, levels = c("dependent", "dependent_source", "independent")),
           experiment = exp,
           additional_sources = n_sources - 1)
  
  claim_pred <- fitted(fit, newdata = newdata_claim, re_formula = NULL, probs = c(0.025, 0.975)) %>%
    as_tibble() %>%
    bind_cols(newdata_claim)
  
  d_predictions <- bind_rows(d_predictions, 
                             pop_pred %>% mutate(level = "population"),
                             claim_pred %>% mutate(level = "claim"))
}

# Plot 1: population-level vs empirical
p_pop <- d_predictions %>%
  filter(level == "population") %>%
  ggplot(aes(x = additional_sources, y = Estimate, color = consensus, fill = consensus)) +
  geom_line(size = 1.1) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), alpha = 0.2, color = NA) +
  geom_point(data = d_empirical_summ, aes(x = additional_sources, y = mean), inherit.aes = FALSE) +
  #geom_errorbar(data = d_empirical_summ, aes(ymin = mean - se, ymax = mean + se),
  #              width = 0.1, inherit.aes = FALSE) +
  facet_grid(~experiment) +
  labs(title = "Population-level (fixed effects only) predictions",
       x = "Consensus Proportion", y = "Confidence") +
  theme_minimal() +
  theme(legend.position = "bottom")

p_pop

# Plot 2: claim-level fits (overlay all claims)
p_claim <- d_predictions %>%
  filter(level == "claim") %>%
  # if many claims, you might want to facet by claim or sample a subset
  ggplot(aes(x = additional_sources, y = Estimate, color = consensus, group = interaction(claim, consensus))) +
  geom_line(alpha = 0.6) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = consensus), alpha = 0.12, color = NA) +
  geom_point(data = d_empirical_summ, aes(x = additional_sources, y = mean, colour = consensus), inherit.aes = FALSE) +
  facet_grid(claim ~ experiment) +   # or facet_wrap(~claim) depending on number of claims
  labs(title = "Claim-specific predictions (includes group effects)",
       x = "Consensus Proportion", y = "Confidence") +
  theme_minimal() +
  theme(legend.position = "bottom")

# show them
p_pop
p_claim
