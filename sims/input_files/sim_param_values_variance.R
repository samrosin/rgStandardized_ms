# Common parameter values across all simulation scenarios.
# If desired, these parameter values could vary across scenarios. 

set.seed(2021)
sigma_e <- c(.95) #true sensitivity
sigma_p <- c(.95,1) #true specificity
n_1 <- c(30,300) #sensitivity study sample size
n_2 <- c(30,300,3000) #specificity study sample size
n_3 <- c(500,5000) #main study sample size

# 
alpha_level <- .05 # alpha level; 1-alpha = confidence level

# Parameters for the logistic model that determines 
# stratum-specific prevalence in scenario 3.
# Note that the intercept alpha_0 is a vector of 3 values, 
# corresponding to the three desired marginal prevalences. 

alpha_0 <- c(-5.268616,-2.8805525,-.5830585) ### these lead to marginal prevalences of .005, .05, and .3
alpha_1 <- -1
alpha_2 <- -.6
alpha_3 <- .8
alpha_4 <- .6
alpha_5 <- .4

# Parameters for the logistic model that determines 
# stratum-specific prevalence in scenario 4.
# Note that the intercept nu_0 is a vector of 3 values, 
# corresponding to the three desired marginal prevalences. 

nu_0 <- c(-5.3445212,-2.9563234,-0.658353) ### these lead to marginal prevalences of .005, .05, and .3
nu_1 <- -1
nu_2 <- -.6
nu_3 <- .8
nu_4 <- .6
nu_5 <- .4
nu_6 <- .1

