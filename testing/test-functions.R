
start = 1.5
asymptote = 80
rate = 0.5
n_sources = c(1,4,7,10)                              
exp = asymptote + start * (1 - exp(-rate * n_sources))
power = asymptote - start * (n_sources^-rate)
log = asymptote/(1 + start * exp(-rate*n_sources))
plot(n_sources, exp, "l")                              
plot(n_sources, power, "l")                              
plot(n_sources, log, "l")                              
