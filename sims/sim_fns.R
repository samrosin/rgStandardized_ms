# Functions helpful for conducting the simulation study. 

# based on true parameters, generates data and returns data neeeded for analysis
gen_data_dgp1 <- function(n_1, sigma_e, n_2, sigma_p, n_3, prev){
  
  # (1) Run the two validation studies
  sigma_e_hat <- rbinom(1 , n_1, sigma_e) / n_1 #estimated sensitivity
  sigma_p_hat <- 1 - rbinom(1, n_2, 1 - sigma_p) / n_2 #estimated specificity
  
 
  # (2) Generate a sample of size n_3 where each person's serostatus is distributed Bern(\pi)
  y <- rbinom(n = n_3, size = 1, prob = prev)
  
  # (3) Generate test results x based on Y, sigma_e, and sigma_p
  sample <- data.frame(y, x = NA)
  sample[sample$y==1,]$x <- as.integer(
    rbinom(n = nrow(sample[sample$y==1,]), size = 1, sigma_e)
    ) #positives test positive with probability sigma_e
  sample[sample$y==0,]$x <- as.integer(
    rbinom(n = nrow(sample[sample$y==0,]), size = 1, 1 - sigma_p)
  )  #negatives test positive with probability 1 - sigma_p
  
  # (4) Return sample estimates
  rho_hat <- mean(sample$x) 
  data <- list(sigma_e_hat = sigma_e_hat, 
               sigma_p_hat = sigma_p_hat, 
               rho_hat = rho_hat)
  data
}

# based on true parameters, generates data and returns data neeeded for analysis
gen_data_dgp2 <- function(n_1, sigma_e, n_2, sigma_p, n_3, stratum_props){
  
  # (1) Run the two validation studies
  sigma_e_hat <- rbinom(1 , n_1, sigma_e) / n_1 #estimated sensitivity
  sigma_p_hat <- 1 - rbinom(1, n_2, 1 - sigma_p) / n_2 #estimated specificity
  
  # (2) Create main study sample based on sampling probs s_js 
  sample <- stratum_props[sample(
    nrow(stratum_props), n_3, replace = T, prob = stratum_props$sampling_prob),]
  
  # (3) Each person's true serology status is distributed Bernoulli
  #     with a stratum-specific mean (according to the vector sample$prev)
  sample$y <- rbinom(n = n_3, size = 1, prob = sample$prev)
  
  # (4) The test results are generated based on Y, sigma_e, and sigma_p
  sample$x <- NA
  sample[sample$y==1,]$x <- as.integer(
    rbinom(n = nrow(sample[sample$y==1,]), size = 1, sigma_e)
    ) #positives test positive with probability sigma_e
  sample[sample$y==0,]$x <- as.integer(
    rbinom(n = nrow(sample[sample$y==0,]), size = 1, 1 - sigma_p) 
    ) #negatives test positive with probability 1 - sigma_p
  
  # (5) Return sample and sample estimates
  rho_hat <- mean(sample$x) 
  data <- list(sigma_e_hat = sigma_e_hat, sigma_p_hat = sigma_p_hat, 
               rho_hat = rho_hat, sample = sample)
  data
}

# The inverse logit function 
inv.logit <- function(x){1/(1 + exp(-x))} 

gen_data_dgp3_4 <- function(n_1, sigma_e, n_2, sigma_p, 
                          n_3, stratum_props, vars_std){
  # (1) Run the two validation studies
  sigma_e_hat <- rbinom(1 , n_1, sigma_e) / n_1 #estimated sensitivity
  sigma_p_hat <- 1 - rbinom(1, n_2, 1 - sigma_p) / n_2 #estimated specificity
  
  # (2) Create main study sample based on sampling probs s_js 
  sample <- stratum_props[sample(nrow(stratum_props), n_3, 
                                 replace = T, prob = stratum_props$sampling_prob),]
  
  # (3) Each person's test result is distributed Bernoulli
  #     with a stratum-specific mean P(X = 1 | Z) 
  sample$x <- rbinom(n = n_3, size = 1, prob = sample$prev_x_z)
  
  # (4) Return sample and sample estimates
  rho_hat <- mean(sample$x) 
  data <- list(sigma_e_hat = sigma_e_hat, sigma_p_hat = sigma_p_hat, 
               rho_hat = rho_hat, sample = sample)
  data
}

# based on true parameters, generates data and returns data needed for analysis.
# relevant for both DGPs 5 and 6. 
gen_data_dgp5_6 <- function(n_1, sigma_e, n_2, sigma_p, 
            n_3, stratum_props, vars_std){
  
  # (1) Run the two validation studies
  sigma_e_hat <- rbinom(1 , n_1, sigma_e) / n_1 #estimated sensitivity
  sigma_p_hat <- 1 - rbinom(1, n_2, 1 - sigma_p) / n_2 #estimated specificity
  
  # (2) Create main study sample based on sampling probs s_js 
  sample <- stratum_props[sample(nrow(stratum_props), n_3, 
                    replace = T, prob = stratum_props$sampling_prob),]
  
  # (3) Each person's true serology status is distributed Bernoulli
  #     with a stratum-specific mean (according to the vector sample$prev)
  sample$y <- rbinom(n = n_3, size = 1, prob = sample$prev)
  
  # (4) The test results are generated based on Y, sigma_e, and sigma_p
  sample$x <- NA
  sample[sample$y==1,]$x <- as.integer(
    rbinom(n = nrow(sample[sample$y==1,]), size = 1, sigma_e)
  ) #positives test positive with probability sigma_e
  sample[sample$y==0,]$x <- as.integer(
    rbinom(n = nrow(sample[sample$y==0,]), size = 1, 1 - sigma_p) 
  ) #negatives test positive with probability 1 - sigma_p
  
  # (5) Return sample and sample estimates
  rho_hat <- mean(sample$x) 
  data <- list(sigma_e_hat = sigma_e_hat, sigma_p_hat = sigma_p_hat, 
               rho_hat = rho_hat, sample = sample)
  data
}

# truncate a number into the range [0, 1]
truncate_01 <- function(x) {min(1, max(0, x))}



