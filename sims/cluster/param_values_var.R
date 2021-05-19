# Common parameter values across all simulation scenarios.
# If desired, these parameter values could vary across scenarios. 

#set.seed(2021)

# a couple values for testing
#sigma_p = .99; sigma_e = .99; n_1 = 40
#n_2 <- 250; n_3 <- 2500

sigma_p <- c(.8, .95, .99)
sigma_e <- c(.8, .99)
n_1 <- 40
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

# for {.01, .02, ..., .20} use the following intercepts: 
# nu_0 <- c(-6.9, -6.18, -5.75, -5.4, -5.2, 
#           -5, -4.8, -4.6, -4.45, -4.35,
#           -4.23, -4.13, -4, -3.88, -3.78,
#           -3.68, -3.58, -3.5, -3.42, -3.32)

# for {.005, .01, .015, ... .20} use the following intercepts:
nu_0 <- c(-7.6, -6.9, -6.45, -6.18, -5.9, -5.75,
          -5.58, -5.41, -5.3, -5.18, -5.06, -4.97, 
          -4.89, -4.79, -4.705, -4.62, -4.54, -4.45, 
          -4.4, -4.35, -4.29, -4.23, -4.18, -4.13,
          -4.06, -4, -3.94, -3.88, -3.83, -3.78,
          -3.735, -3.69, -3.642, -3.596, -3.55, -3.505,
          -3.462, -3.42, -3.375, -3.337)
nu_1 <- -1
nu_2 <- 3.25 #nu_2 = 5? 
nu_3 <- .8
nu_4 <- .6
nu_5 <- .4
nu_6 <- .1

