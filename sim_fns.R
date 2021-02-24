# author: Sam Rosin
# this file contains functions used in conducting the simulation study

#' Compute a Rogan-Gladen point estimate. Corresponds to \hat \pi in the manuscript.
#'
#' @param rho_hat Raw/apparent prevalence (positives divided by total samples)
#' @param sigma_e_hat Estimated sensitivity 
#' @param sigma_p_hat Estimated specificity
#' @param n_1 Sample size for sensitivity validation dataset
#' @param n_2 Sample size for specificity validation dataset
#' @param n_3 Sample size for main study from population
#' @param variance Should variance be estimated or not? 
#' 
#' @return Rogan-Gladen estimated prevalence; optionally, a vector also including variance estimate 
#' @examples
#' ests_rg(0.01, .99, .98, 30, 50, 1000, TRUE)
#' 

ests_rg <- function(rho_hat, sigma_e_hat, sigma_p_hat, n_1, n_2, n_3, variance = FALSE){
  pi_hat <- (rho_hat + sigma_p_hat - 1) / (sigma_e_hat + sigma_p_hat - 1) ###Rogan and Gladen formula
  if (variance == FALSE) {
    return(pi_hat)
  } else {
    # compute variance estimator
    a <- pi_hat^2 * sigma_e_hat * (1 - sigma_e_hat) / n_1
    b <- (1 - pi_hat)^2 * sigma_p_hat * (1 - sigma_p_hat) / n_2
    c <- rho_hat * (1 - rho_hat) / n_3
    var_pi_hat <- (a + b + c) * (sigma_e_hat + sigma_p_hat - 1)^(-2)
    return(c(pi_hat, var_pi_hat))
  }
}

#' Compute a point estimate of prevalence using standardization and a Rogan-Gladen adjustment. 
#' Corresponds to \hat \pi_{st} in the manuscript. 
#'
#' @param sample A dataframe with data from the sample, including the stratum_props (gamma_js) which will be used in standardization
#' @param sigma_e_hat Estimated sensitivity 
#' @param sigma_p_hat Estimated specificity
#' @param n_1 Sample size for sensitivity validation dataset
#' @param n_2 Sample size for specificity validation dataset
#' @param vars_std The names of variables to standardize over, corresponding to columns in sample
#' @param variance Should variance be estimated?
#'
#' @return Estimated prevalence; optionally, a vector also including variance estimate
#' @export
#'
#' @examples point_est_std(sample, sigma_e_hat, sigma_p_hat, n_1, n_2, vars_std)
ests_std <- function(sample, sigma_e_hat, sigma_p_hat, n_1, n_2, n_3, vars_std, variance = FALSE){
  #unique covariate strata in the sample data
  strata <- unique(sample[,c(vars_std,"stratum_prop")])
  
  #the stratum proportions (\gamma_js) have to first be redefined/recomputed to match the target dataset
  strata$stratum_prop <- strata$stratum_prop / sum(strata$stratum_prop)
  
  #strata, with number and proportion of positive tests
  strata_npos <- sample %>% dplyr::group_by_at(vars_std) %>%
    dplyr::summarise(n = n(), n_pos = sum(x), .groups = "drop")
  
  #join the two datasets and make the standardization estimates, then sum them.
  #the formula is std_est = \hat \rho_j * \gamma_j 
  strata <- dplyr::inner_join(strata, strata_npos, by = vars_std)
  
  strata$std_est <- (cells$n_pos / cells$n) * cells$stratum_prop
  pi_hat_st <- (sum(strata$std_est) - (1-sigma_p_hat))/(sigma_e_hat - (1-sigma_p_hat)) #point estimate
  
  if(variance == TRUE){
    return(pi_hat_st)
  } else{ 
    # compute variance estimator by summing the three components. compare to formula in manuscript. 
    a <- pi_hat_std^2 * sigma_e_hat * (1 - sigma_e_hat) / n_1
    b <- (1 - pi_hat_std)^2 * sigma_p_hat * (1 - sigma_p_hat) / n_2
    # c is the third component of the variance estimator. c = (gamma_j^2*\hat \rho_j * (1- \hat \rho_j))/(n_{z_j})
    strata <- strata %>% mutate(
      c = stratum_prop^2 * (n_pos / n) * (1 - (n_pos / n) ) / n
      )
    c <- sum(strata$c)
    var_pi_hat_st <- (a + b + c) * (sigma_e_hat + sigma_p_hat - 1)^(-2)
  }
  
  #return point and variance estimates
  c(pi_hat_st, var_pi_hat_st)
}

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
