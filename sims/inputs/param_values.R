# Common parameter values across all simulation scenarios.
# If desired, these parameter values could vary across scenarios. 

set.seed(2021)

#sigma_p = .95; sigma_e = .8; n_1 = 40 # a couple values for testing

sigma_p <- c(.8, .95, .99)
sigma_e <- c(.8, .99)
n_1 <- 40 #n_1 <- c(40, 250)
n_2 <- 250
n_3 <- 2500

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
#alpha_0 <- c(-5.268616,-2.8805525,-.5830585) ### these lead to marginal prevalences of .005, .05, and .3
alpha_1 <- -1
alpha_2 <- -.6
alpha_3 <- .8
alpha_4 <- .6
alpha_5 <- .4

# Parameters for the logistic model that determines 
# stratum-specific prevalence in scenario 4.
# Note that the intercept nu_0 is a vector 
# corresponding to the desired marginal prevalences. 
nu_0 <- c(-7, -6.2, -5.7, -5.4, -5.2, 
          -5, -4.8, -4.6, -4.45, -4.35,
          -4.23, -4.13, -4, -3.88, -3.78,
          -3.68, -3.58, -3.5, -3.42, -3.32)
nu_1 <- -1
nu_2 <- 3.25 
nu_3 <- .8
nu_4 <- .6
nu_5 <- .4
nu_6 <- .1

