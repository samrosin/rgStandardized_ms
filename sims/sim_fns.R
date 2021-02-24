# Functions helpful for conducted the simulation study. 

gen_data_scenario1 <- function(n_1, sigma_e, n_2, sigma_p, n_3, prev){
  #### (1) Run the two validation studies
  sigma_e_hat <- rbinom(1,n_1,sigma_e)/n_1 #estimated sensitivity
  sigma_p_hat <-1-rbinom(1,n_2,1-sigma_p)/n_2 #estimated specificity
  
  ### (2) Generate the sample from the population based on Y, sigma_e, sigma_p, and prev 
  y <- rbinom(n = n_3, size = 1, prob = prev)
  sample <- data.frame(y, x=NA)
  sample[sample$y==1,]$x <- as.integer(
    rbinom(n = nrow(sample[sample$y==1,]), size = 1, sigma_e)) #positives test positive with probability sigma_e
  sample[sample$y==0,]$x <- as.integer(
    rbinom(n = nrow(sample[sample$y==0,]), size = 1, 1-sigma_p) #negatives test positive with probability 1-sigma_p
  )
  
  rho_hat <- mean(sample$x) 
  data <- list(sigma_e_hat = sigma_e_hat, sigma_p_hat = sigma_p_hat, rho_hat = rho_hat)
  data
}