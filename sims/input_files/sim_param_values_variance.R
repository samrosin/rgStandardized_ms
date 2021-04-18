# Common parameter values across all simulation scenarios.
# If desired, these parameter values could vary across scenarios. 

set.seed(2021)
sigma_p <- c(.8, .95, .99)
sigma_e <- c(.8, .99)
n_1 <- c(40, 250)
n_2 <- 250
n_3 <- 2500

### TESTING
# sigma_e <- c(.95) #true sensitivity
# sigma_p <- c(.95) #true specificity
# n_1 <- c(300) #sensitivity study sample size
# n_2 <- c(300,3000) #specificity study sample size
# n_3 <- c(5000) #main study sample size

alpha_level <- .05 # alpha level; 1-alpha = confidence level

# Parameters for the logistic model that determines 
# stratum-specific prevalence in scenario 3.
# Note that the intercept alpha_0 is a vector, 
# corresponding to the desired marginal prevalences. 
# Also note these are denoted "beta" in the manuscript. 

alpha_0 <- c(-4.575, -3.85, -3.429, -3.127, -2.8805525, 
             -2.68, -2.505, -2.355, -2.215, -2.08,
             -1.975, -1.87, -1.77, -1.675, -1.585,
             -1.5, -1.42, -1.345, -1.27, -1.2
)
alpha_1 <- -1
alpha_2 <- -.6
alpha_3 <- .8
alpha_4 <- .6
alpha_5 <- .4

# Parameters for the logistic model that determines 
# stratum-specific prevalence in scenario 4.
# Note that the intercept nu_0 is a vector 
# corresponding to the desired marginal prevalences. 
nu_0 <- c(-4.64, -3.93, -3.505, -3.2, -2.9563234, 
          -2.75, -2.58, -2.44, -2.291, -2.17,
          -2.05, -1.94, -1.85, -1.755, -1.67,
          -1.58, -1.5, -1.42, -1.345, -1.27
)
#nu_0 <- c(-5.3445212,-2.9563234,-0.658353) ### these lead to marginal prevalences of .005, .05, and .3
nu_1 <- -1
nu_2 <- -.6
nu_3 <- .8
nu_4 <- .6
nu_5 <- .4
nu_6 <- .1

