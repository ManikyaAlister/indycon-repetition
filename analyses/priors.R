generate_power_priors <- function(data, baseline = "dependent", model = model_name) {
  
  # Ensure consensus is a factor and get its levels
  consensus_levels <- levels(factor(data$consensus))
  consensus_levels <- setdiff(consensus_levels, baseline)  # drop baseline level
  
  # Helper to create prior for one term
  make_prior <- function(nlpar, coef, mean_val, sd_val) {
    eval(bquote(prior(normal(.(mean_val), .(sd_val)), nlpar = .(nlpar), coef = .(coef))))
  }
  
  # Using exponentiation approach (keep your original model formulas with exp())
  # Priors are on LOG SCALE, but tighter than before for better identifiability
  
  # Start with intercept priors on log scale
  priors <- c(
    # log(increase): log(25) ≈ 3.22, SD reduced from 0.5 to 0.3
    make_prior("increase", "Intercept", log(25), 0.3),
    
    # log(start): log(50) ≈ 3.91, SD reduced from 0.5 to 0.3
    make_prior("start", "Intercept", log(50), 0.3),
    
    # log(rate): log(0.5) ≈ -0.69, SD reduced from 0.2 to 0.15
    make_prior("rate", "Intercept", log(0.5), 0.15)
  )
  
  # Check which parameters vary by consensus in this model
  has_increase_consensus <- grepl("increase", model_name) | grepl("full", model_name)
  has_start_consensus <- grepl("start", model_name) | grepl("full", model_name)
  has_rate_consensus <- grepl("rate", model_name) | grepl("full", model_name)
  
  # Add consensus effect priors
  # These are on the LOG SCALE (differences in log parameters)
  # Tighter priors for better regularization
  for (lvl in consensus_levels) {
    coef_name <- paste0("consensus", lvl)
    
    if (has_increase_consensus) {
      # On log scale, ±0.2 represents roughly ±22% change in real value
      priors <- c(priors, make_prior("increase", coef_name, 0, 0.2))
    }
    
    if (has_start_consensus) {
      # On log scale, ±0.2 represents roughly ±22% change in real value
      priors <- c(priors, make_prior("start", coef_name, 0, 0.2))
    }
    
    if (has_rate_consensus) {
      # Rate is more sensitive, keep tighter prior
      priors <- c(priors, make_prior("rate", coef_name, 0, 0.15))
    }
  }
  
  # Add random effect (sd) priors for models with random effects
  if (grepl("re_", model_name) | grepl("hierarchical", model_name)) {
    # Random effects are on LOG SCALE
    # Tighter priors for better regularization and identifiability
    
    # SD for start random effects (on log scale)
    priors <- c(priors, 
                prior(normal(0, 0.1), class = "sd", nlpar = "start", lb = 0))
    
    # SD for increase random effects (on log scale)
    priors <- c(priors,
                prior(normal(0, 0.1), class = "sd", nlpar = "increase", lb = 0))
    
    # SD for rate random effects (on log scale)
    priors <- c(priors,
                prior(normal(0, 0.08), class = "sd", nlpar = "rate", lb = 0))
    
    # If the model has random slopes (full_hierarchical), add correlation priors
    if (grepl("full_hierarchical", model_name)) {
      # LKJ prior for correlation matrices (eta = 2 is weakly informative)
      priors <- c(priors,
                  prior(lkj(2), class = "cor", nlpar = "start"),
                  prior(lkj(2), class = "cor", nlpar = "increase"),
                  prior(lkj(2), class = "cor", nlpar = "rate"))
    }
  }
  
  # Add residual error prior
  priors <- c(priors,
              prior(normal(0, 10), class = "sigma", lb = 0))
  
  priors
}