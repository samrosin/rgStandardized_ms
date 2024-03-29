# Parameter values for dgp4

# sigma_p = .95; sigma_e = .8; n_1 = 40 # a couple values for testing
sigma_p <- c(.8, .95, .99)
sigma_e <- c(.8, .99)
n_1 <- 40
n_2 <- 250
n_3 <- 2500
alpha_level <- .05 # 1 - alpha = confidence level

# Parameters for the logistic model that determines 
# stratum-specific prevalence in scenario 4.
# Note that the intercept nu_0 is a vector, 
# corresponding to the desired marginal prevalences. 
nu_0 <- c(-3.2855, -3.2365, -3.188, -3.1405, -3.0935, 
          -3.047, -3.001, -2.9555, -2.9105, -2.866, 
          -2.822, -2.778, -2.7345, -2.6915, -2.6487, 
          -2.6063, -2.564, -2.5223, -2.4805, -2.4393, # sigma_e = sigma_p = .8
          
          -5.021, -4.877, -4.746, -4.626, -4.515, 
          -4.41, -4.312, -4.219, -4.13, -4.045, 
          -3.964, -3.8855, -3.81, -3.7365, -3.665, 
          -3.596, -3.5285, -3.4625, -3.398, -3.335, # sigma_e = .8, sigma_p = .95
          
          -6.3, -5.91, -5.623, -5.39, -5.193, 
          -5.023, -4.872, -4.735, -4.609, -4.493, 
          -4.385, -4.283, -4.186, -4.0945, -4.007, 
          -3.923, -3.842, -3.7635, -3.688, -3.614, # sigma_e = .8, sigma_p = .99
          
          -3.27, -3.206, -3.143, -3.081, -3.02, 
          -2.96, -2.901, -2.8425, -2.7845, -2.7275, 
          -2.671, -2.615, -2.559, -2.504, -2.4495, 
          -2.3955, -2.3415, -2.2885, -2.235, -2.1825, # sigma_e = .99, sigma_p = .8
          
          -4.984, -4.81, -4.654, -4.513, -4.383, 
          -4.263, -4.15, -4.043, -3.942, -3.845, 
          -3.752, -3.662, -3.576, -3.492, -3.411, 
          -3.332, -3.254, -3.1783, -3.1045, -3.0315, # sigma_e = .99, sigma_p = .95
          
          -6.19, -5.765, -5.450, -5.2, -4.992,
          -4.81, -4.648, -4.502, -4.368, -4.243,
          -4.127, -4.017, -3.912, -3.813, -3.717,
          -3.625, -3.5365, -3.4505, -3.367, -3.285) # sigma_e = sigma_p = .99

nu_1 <- -1
nu_2 <- 3.25 
nu_3 <- .8
nu_4 <- .6
nu_5 <- .4
nu_6 <- .1
