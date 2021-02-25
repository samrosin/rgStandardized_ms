# Common parameter values across all simulation scenarios.
# If desired, these parameter values could vary across scenarios. 

set.seed(2021)
sigma_e <- c(.7,.95,1) #true sensitivity
#sigma_p <- .95
n_1 <- c(30,300)
n_2 <- c(30,300)
sigma_p <- c(.7,.95,1) #true specificity
#n_1 <- c(30,300,3000) #sensitivity study sample size
#n_2 <- c(30,300,3000) #specificity study sample size
n_3 <- c(500,5000,20000) #main study sample size 
