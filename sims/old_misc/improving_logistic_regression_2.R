f1 <- function(coef){
  d <- matrix(0, nrow = 2, ncol = nrow(coef)) # declare D as 2 * p matrix of 0s (convenient since the 2nd row is 0)
  for(l in 1:nrow(coef)){
    stratum_contrib <- rep(NA, nrow(mat_strat)) # contribution of the jth stratum to the sum
    exp_coef_times_mat_strat <- exp(crossprod(coef, t(mat_strat))) # this is a row vector with ith element exp(hat beta ^T %*% h(Z_i))
    for(j in 1:nrow(mat_strat)){
      stratum_contrib[j] <- as.matrix(mat_strat[j,l]) * exp_coef_times_mat_strat[1,j] / 
        (1 + exp_coef_times_mat_strat[1,j])^2 * strata$stratum_prop[j]
    }
    d[1,l] <- -1 * sum(stratum_contrib)
  }
  d
}

f2 <- function(coef){
  d <- matrix(0, nrow = 2, ncol = nrow(coef)) # declare D as 2 * p matrix of 0s (convenient since the 2nd row is 0)
  for(l in 1:nrow(coef)){
    stratum_contrib <- rep(NA, nrow(mat_strat)) # contribution of the jth stratum to the sum
    for(j in 1:nrow(mat_strat)){
      stratum_contrib[j] <- as.matrix(mat_strat[j,l]) * exp(crossprod(coef, mat_strat[j,])) / 
        (1 + exp(crossprod(coef, mat_strat[j,])))^2 * strata$stratum_prop[j]
    }
    d[1,l] <- -1 * sum(stratum_contrib)
  }
  d
}
identical(f1(coef), f2(coef))


bench::mark(
  f1(coef), 
  f2(coef)
)[c("expression", "min", "median", "itr/sec", "n_gc")]


