# Common parameter values across all simulation scenarios.
# If desired, these parameter values could vary across scenarios. 

set.seed(2021)
#sigma_e <- c(.95) #true sensitivity
sigma_e <- c(.7, .95, 1)
#sigma_p <- .95
sigma_p <- c(.7, .95, 1)
#n_1 <- c(30,300)
#n_2 <- c(30,300)
#n_3 <- c(500,5000) #main study sample size 
n_1 <- c(30,300,3000) #sensitivity study sample size
n_2 <- c(30,300,3000) #specificity study sample size
n_3 <- c(500,5000,20000) #main study sample size 

# Parameters for the logistic model that determines 
# stratum-specific prevalence in scenario 3.
# Note that the intercept alpha_0 is a vector of 3 values, 
# corresponding to the three desired marginal prevalences. 

alpha_0 <- c(-5,-2.6375,-0.31) ### these lead to marginal prevalences of .005, .05, and .3
alpha_1 <- -1
alpha_2 <- -.6
alpha_3 <- .8
alpha_4 <- .6
alpha_5 <- .4
