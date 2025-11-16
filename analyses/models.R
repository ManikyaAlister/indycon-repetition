models <- list(
  "full" =  bf(
    # I need to make surethat the parameters can never be < 0. Conventionally, 
    # you would do this by setting a lower bound in the priors, but this will also 
    # restrict the difference between independent and dependent to be >0 for all 
    # parameters, which I do not want, so that I can identify cases where there are no real differences. 
    # You still need to set some kind of restriction, though, because otherwise the model will have trouble converging. 
    # My solution is to exponentiation each parameter, meaning that they are functionally constrained to be positive. 
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + consensus,# + prior_belief,
    increase ~ 1 + consensus, #+ prior_belief,
    rate ~ 1 + consensus,#+ prior_belief,
    nl = TRUE
    
  ),
  # remove asymptote (increase parameter) varying by consensus
  "start_rate" =  bf( 
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + consensus,# + prior_belief,
    increase ~ 1, #+ consensus, # + prior_belief,
    rate ~ 1 + consensus, #+ prior_belief,
    nl = TRUE
  ),
  "start_increase" =  bf( 
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + consensus,
    increase ~ 1 + consensus,
    rate ~ 1,
    nl = TRUE
  ),
  "rate_increase" =  bf( 
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1,
    increase ~ 1 + consensus,
    rate ~ 1 + consensus,
    nl = TRUE
  ),
  "rate" =  bf( 
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1,
    increase ~ 1, 
    rate ~ 1 + consensus, 
    nl = TRUE
  ),
  "start" =  bf( 
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + consensus,
    increase ~ 1, 
    rate ~ 1,
    nl = TRUE
  ),
  "increase" =  bf( 
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1,
    increase ~ 1 + consensus, 
    rate ~ 1, 
    nl = TRUE
  ),
  "none" =  bf( 
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1,
    increase ~ 1, 
    rate ~ 1, 
    nl = TRUE
  ),
  "full_hierarchical" = bf(
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    # Each parameter varies by consensus (fixed effect) 
    # and by claim (hierarchical random + fixed effect)
    start    ~ 1 + consensus + (1 + consensus | claim),
    increase ~ 1 + consensus + (1 + consensus | claim),
    rate     ~ 1 + consensus + (1 + consensus | claim),
    nl = TRUE
  ),
  # Random effects versions
  "re_claim_full" = bf(
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + consensus + (1 | claim),
    increase ~ 1 + consensus + (1 | claim),
    rate ~ 1 + consensus + (1 | claim),
    nl = TRUE
  ),
  "re_claim_start_rate" = bf(
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + consensus + (1 | claim),
    increase ~ 1 + (1 | claim),
    rate ~ 1 + consensus + (1 | claim),
    nl = TRUE
  ),
  "re_claim_start_increase" = bf(
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + consensus + (1 | claim),
    increase ~ 1 + consensus + (1 | claim),
    rate ~ 1 + (1 | claim),
    nl = TRUE
  ),
  "re_claim_rate_increase" = bf(
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + (1 | claim),
    increase ~ 1 + consensus + (1 | claim),
    rate ~ 1 + consensus + (1 | claim),
    nl = TRUE
  ),
  "re_claim_rate" = bf(
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + (1 | claim),
    increase ~ 1 + (1 | claim),
    rate ~ 1 + consensus + (1 | claim),
    nl = TRUE
  ),
  "re_claim_start" = bf(
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + consensus + (1 | claim),
    increase ~ 1 + (1 | claim),
    rate ~ 1 + (1 | claim),
    nl = TRUE
  ),
  "re_claim_increase" = bf(
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + (1 | claim),
    increase ~ 1 + consensus + (1 | claim),
    rate ~ 1 + (1 | claim),
    nl = TRUE
  ),
  "re_claim_none" = bf(
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + (1 | claim),
    increase ~ 1 + (1 | claim),
    rate ~ 1 + (1 | claim),
    nl = TRUE
  ),
  # Random effects versions with valence
  "re_valence_full" = bf(
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + consensus + (1 | valence),
    increase ~ 1 + consensus + (1 | valence),
    rate ~ 1 + consensus + (1 | valence),
    nl = TRUE
  ),
  "re_valence_start_rate" = bf(
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + consensus + (1 | valence),
    increase ~ 1 + (1 | valence),
    rate ~ 1 + consensus + (1 | valence),
    nl = TRUE
  ),
  "re_valence_start_increase" = bf(
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + consensus + (1 | valence),
    increase ~ 1 + consensus + (1 | valence),
    rate ~ 1 + (1 | valence),
    nl = TRUE
  ),
  "re_valence_rate_increase" = bf(
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + (1 | valence),
    increase ~ 1 + consensus + (1 | valence),
    rate ~ 1 + consensus + (1 | valence),
    nl = TRUE
  ),
  "re_valence_rate" = bf(
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + (1 | valence),
    increase ~ 1 + (1 | valence),
    rate ~ 1 + consensus + (1 | valence),
    nl = TRUE
  ),
  "re_valence_start" = bf(
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + consensus + (1 | valence),
    increase ~ 1 + (1 | valence),
    rate ~ 1 + (1 | valence),
    nl = TRUE
  ),
  "re_valence_increase" = bf(
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + (1 | valence),
    increase ~ 1 + consensus + (1 | valence),
    rate ~ 1 + (1 | valence),
    nl = TRUE
  ),
  "re_valence_none" = bf(
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + (1 | valence),
    increase ~ 1 + (1 | valence),
    rate ~ 1 + (1 | valence),
    nl = TRUE
  ),
  "fe_valence_full" = bf(
    confidence ~ (exp(start) + exp(increase)) - exp(increase) * (n_sources^(-exp(rate))),
    start ~ 1 + consensus + valence,
    increase ~ 1 + consensus + valence,
    rate ~ 1 + consensus + valence,
    nl = TRUE
  )
  
)