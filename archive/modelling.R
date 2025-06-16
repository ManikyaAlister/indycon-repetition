rm(list = ls())
library(here)
library(tidyverse)
library(rstan)
library(loo)

experiment <- 2
data <- read.csv(here(paste0("data/experiment-",experiment,"/clean/con_rep_Exp",experiment,"_cogsci.csv")))

data <- data %>%
  mutate(
    n_sources = case_when(
      repetitions == "Baseline" ~ 1,
      repetitions == "Phase 1" ~ 4,
      repetitions == "Phase 2" ~ 7,
      repetitions == "Phase 3" ~ 10
    )
  )

# save transformed data
save(data, file = here(paste0("data/experiment-",experiment,"/clean/d-n-repetitions.Rdata")))


# define functional forms
forms = list(
  exp <- list(
    code = "a + b * (1 - exp(-c * n_sources))",
    fun = function(data) {
      a + b * (1 - exp(-c * data$n_sources))
    }
  ),
  
  power <- list(
    code =  "a + b * (n_sources ^-c)",
    fun = function(data) {
      a + b * (data$n_sources ^ -c)
    }
  ),
  
  log <- list(
    code = "a ./ (1 + b * exp(-c * n_sources))",
    fun = function(data) {
      a / (1 + b * exp(-c * data$n_sources))
    }
  )
)


nonLinearRegression = function(form, d_model) {
  code = form$code
  fun = form$fun
  
  stan_data = "data {
  int<lower=1> N;
  vector[N] n_sources;
  vector[N] confidence;
}"
  
  stan_parameters = "parameters {
  real a;                  // Baseline confidence
  real<lower=0> b;          // Scaling factor
  real<lower=0> c;         // Decay rate
  real<lower=0> sigma;     // Noise standard deviation
}"
  
  stan_model = paste0("model {
  vector[N] mu;
  mu = ",
                      code,
                      ";
  confidence ~ normal(mu, sigma);")
  
  stan_priors = " a ~ normal(50, 25);
  b ~ normal(30, 15);
  c ~ normal(0.3, 0.1);
  sigma ~ normal(10, 5);
}"
  
  stan_generated_quantities = paste0(
    "generated quantities {
  vector[N] log_lik;
  vector[N] mu;
  mu = ",
    code,
    ";
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(confidence[n] | mu[n], sigma);
  }
}"
  )
  
  
  stan_code =  paste0(stan_data,
                      stan_parameters,
                      stan_model,
                      stan_priors,
                      stan_generated_quantities)
  
  # Prepare data
  stan_data <- list(
    N = nrow(d_model),
    n_sources = d_model$n_sources,
    confidence = d_model$confidence
  )
  
  # Compile and fit the model
  fit <- stan(
    model_code = stan_code,
    data = stan_data,
    iter = 2000,
    chains = 4,
    seed = 1234,
    cores = 4
  )
  fit
}

# Stan model defined as a string in R

conditions <- c("independent", "dependent")
funs <- c("exp")#, "power", "log")
model_combos <- expand.grid("functions" = funs, "condition" = conditions)

# empty list to store
all_output = list()

# loop through each condition
for (i in 1:nrow(model_combos)) {
  # extract condition
  cond <- model_combos[i, "condition"]
  func <- model_combos[i, "functions"]
  
  run_name <- paste0(cond, "+", func)
  
  form = forms[[func]]
  
  d_model <- data %>%
    filter(consensus == cond)
  
  fit <- nonLinearRegression(form, d_model)
  looic_full <- loo(fit) # looic for model comparison
  looic <- looic_full$estimates["looic", "Estimate"]
  looic_se <- looic_full$estimates["looic", "SE"]
  summ_fit <- summary(fit)
  output <- list()
  output$estimates <- summ_fit$summary[c("a", "b", "c", "sigma"), ] # extract parameter estimates
  output$looic <- looic
  output$looic_se <- looic_se
  output$cond <- cond
  output$func <- func
  
  # save without full fit so it's a smaller file
  all_output[[run_name]] <- output
  
  # save individual output with full fit
  output$full_fit <- fit
  save(output, file =  here(paste0("output/", run_name, "-e",experiment,".Rdata")))
  
}

save(all_output, file = here(paste0("output/all-output-e",experiment,".Rdata")))


# # Print results
# print(fit, pars = c("a", "b", "c", "sigma"))
# 
# # Extract posterior means
# posterior_means <- extract(fit)
# a <- mean(posterior_means$a)
# b <- mean(posterior_means$b)
# c <- mean(posterior_means$c)
# # Generate fitted values
# d_plotting$fitted <- form$fun()
# 
# # Plot observed vs fitted
# ggplot(d_plotting, aes(x = n_sources, y = confidence, group = consensus, colour = consensus)) +
#   geom_line() +
#   geom_line(aes(y = fitted), linetype = "dashed")  # Add fitted curve
