
n_sources <- 1:11
a = 43
b = 24
c = 0.36
belief = a + b * (1 - exp(-c * n_sources))

a_log = 74 #a + b   # Match upper asymptote to exponential function
b_log = 0.8 #(a_log / belief[1]) - 1
# Increase to push initial values lower
c_log = 0.45 #     # Increase to steepen the transition

belief_log =   a_log / (1 + b_log * exp(-c_log * n_sources))

plot(n_sources, belief, "l", main = "a + b * (1 - exp(-c * n_sources))", ylim = c(0,100))  
lines(n_sources, belief_log)


n_sources <- 1:200
a = 50
b = 30
c = 0.05
belief = a + b * (1 - exp(-c * n_sources))

a_log = a + b
b_log = 0.6 #(a_log / belief[1]) - 1  # Ensures similar starting value
c_log = c/2.5  # Start with c_log = c, adjust if needed

belief_log = a_log / (1 + b_log * exp(-c_log * n_sources))

plot(n_sources, belief, type = "l", col = "blue", lwd = 2, ylim = c(0, 80),
     main = "Comparison of Functions", ylab = "Belief", xlab = "n_sources")
lines(n_sources, belief_log, col = "red", lwd = 2, lty = 2)
legend("bottomright", legend = c("Exponential", "Logistic-like"),
       col = c("blue", "red"), lty = c(1,2), lwd = 2)

# Load necessary library
library(ggplot2)

# Define range of n_sources
n_sources <- seq(1, 100, length.out = 100)

# Define parameters
a <- 80    # Upper asymptote
b <- 30    # Growth rate scaling
c <- 0.1   # Rate parameter

# 1. Exponential function
belief_exp <- a + b * (1 - exp(-c * n_sources))

# 2. Logistic-like function
belief_log <- a / (1 + b * exp(-c * n_sources))

# 3. Michaelis-Menten function
belief_mm <- (a * n_sources) / (b + n_sources)

# 4. Gompertz function
belief_gomp <- a * exp(-b * exp(-c * n_sources))

# 5. Weibull CDF
belief_weib <- a * (1 - exp(- (b * n_sources)^c))

# 6. Hill function
belief_hill <- (a * n_sources^c) / (b^c + n_sources^c)

# Combine results into a data frame
belief_df <- data.frame(
  n_sources = rep(n_sources, 6),
  belief = c(belief_exp, belief_log, belief_mm, belief_gomp, belief_weib, belief_hill),
  function_type = rep(c("Exponential", "Logistic-like", "Michaelis-Menten",
                        "Gompertz", "Weibull", "Hill"), each = length(n_sources))
)

# Plot all functions
ggplot(belief_df, aes(x = n_sources, y = belief, color = function_type)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "Comparison of Growth Functions",
       x = "n_sources",
       y = "Belief",
       color = "Function Type")
