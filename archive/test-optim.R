

# Define the exponential function
exp_function <- function(params, n_sources, confidence) {
  a <- params[1]
  b <- params[2]
  c <- params[3]
  
  # Model predictions
  pred <- a + b * (1 - exp(-c * n_sources))
  
  # Return sum of squared errors (SSE)
  sum((confidence - pred)^2)
}

# Function to fit model using optim()
fit_exponential <- function(data) {
  # Initial parameter guesses (a, b, c)
  init_params <- c(50, 30, 0.3)
  
  # Use optim() to minimize SSE
  fit <- optim(
    par = init_params,
    fn = exp_function,
    n_sources = data$n_sources,
    confidence = data$confidence,
    method = "L-BFGS-B",
    lower = c(0, 0, 0)  # Ensure parameters remain positive
  )
  
  return(fit$par)  # Return best-fit parameters
}

# Load your data
# Assuming `data` is your dataframe with columns: confidence, consensus, n_sources

# Split the data by condition and fit separately
fits <- data %>%
  group_by(consensus) %>%
  summarise(params = list(fit_exponential(cur_data())))


simExpOptim = function(fits,n_sources){
  sim_data_optim <- NULL 
  for ( i in 1:2) {
    params <- fits$params[[i]] 
    a <- params[1]
    b <- params[2]
    c <- params[3]
    consensus <- fits$consensus[[i]]
    
    d_i <- data.frame(
      model_predictions = a + b * (1 - exp(-c * n_sources)),
      n_sources = n_sources,
      consensus = consensus
    )
    
    sim_data_optim <- rbind(sim_data_optim, d_i)
  }
  
  sim_data_optim
  
}

fits_optim <- simExpOptim(fits, 0:11)


fits_optim %>% 
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
