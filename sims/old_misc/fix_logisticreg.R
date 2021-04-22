# april 14 - fix variance of logistic regression
# 
sample = dat$sample
stratum_props = as.data.frame(row$stratum_props)
sigma_e_hat = dat$sigma_e_hat
sigma_p_hat = dat$sigma_p_hat
mod_formula = formula("x ~ z1_z11 + z2_z20 +
                      z2_z21 + z3_z30 + z3_z31")
vars_std = c("z1_z11", "z2_z20", "z2_z21", "z3_z30", "z3_z31")
variance = TRUE

library(fastDummies)
# ests_std_model_alt <- function(sample, stratum_props, sigma_e_hat,
#                            sigma_p_hat, n_1, n_2, n_3, vars_std,
#                            mod_formula, variance = FALSE){
#   
  # make dummy columns for stratum_props and sample
x_model <- glm(formula = mod_formula, family = binomial, data = sample) #regression model

# make predictions for all strata
stratum_props$model_pred <- predict(x_model, newdata = stratum_props, type = "response")
stratum_props$std_est_model <- stratum_props$model_pred * stratum_props$stratum_prop

# point estimate
pi_hat_mst <- (sum(stratum_props$std_est_model) - (1 - sigma_p_hat)) / 
  (sigma_e_hat - (1 - sigma_p_hat))
pi_hat_mst_trunc <- max(0,min(pi_hat_mst,1)) #truncate into [0,1]

  # if(variance == FALSE){
  #   return(pi_hat_mst_trunc)
  # } else{
    
n <- n_1 + n_2 + n_3 #total sample size

##### 
# estimate the different parts of the variance estimator
coef <- as.matrix(coef(x_model)) # regression coefficients 
mat <- model.matrix(x_model) # design matrix
a <- matrix(c(n_1 / n, 0, 0, n_2 / n), nrow = 2, ncol = 2) # estimate A

# estimate B
b <- matrix(NA, nrow = nrow(coef), ncol = nrow(coef)) # declare B-hat as an empty p * p matrix 
exp_coef_times_mat <- exp(crossprod(coef, t(mat))) # this is a 1 by n matrix with ith element exp(hat beta ^T %*% h(Z_i))
for(j in 1:nrow(coef)){
  for(k in 1:nrow(coef)){
    unit_contrib_b <- rep(NA, nrow(mat)) # each unit's contribution to this jkth element of B
    for(i in 1:ncol(exp_coef_times_mat)){
      unit_contrib_b[i] <- mat[i,j] * mat[i,k] * exp_coef_times_mat[1,i] / 
        (1 + exp_coef_times_mat[1,i])^2
    }
    b[j,k] <- (n_3 / n) * mean(unit_contrib_b)
  }
}

c <- matrix(c(0, pi_hat_mst, 0, -1 + pi_hat_mst), nrow = 2, ncol = 2) # estimate C

# estimate D
# unique covariate strata in the sample data
strata <- unique(sample[,c(vars_std,"stratum_prop")])
covariate_names <- colnames(strata)[1:length(vars_std)]

# create only the RHS of the regression formula, for application to *all* strata, 
model_mat_formula <- "~"
for(cov_name in covariate_names){
  model_mat_formula <- paste(model_mat_formula, "strata$",cov_name,"+",sep="")
}
model_mat_formula <- formula(substr(model_mat_formula,1,nchar(model_mat_formula)-1)) # remove final "+" from formula
mat_strat <- model.matrix(model_mat_formula) # model matrix for *all* strata, even those not in sample support

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

e <- matrix(c(1, -1, 0, sigma_e_hat + sigma_p_hat -1), nrow = 2, ncol = 2) # estimate E
f <- matrix(c(n_1 * sigma_e_hat * (1 - sigma_e_hat) / n, 0,
              0, n_2 * sigma_p_hat * (1 - sigma_p_hat) / n), nrow = 2, ncol = 2) # estimate F 

# estimate G 
g <- matrix(NA, nrow = nrow(coef), ncol = nrow(coef)) #declare G as an empty p * p matrix 
invlogit_coef_times_mat <- inv.logit(crossprod(coef, t(mat))) # 1 times n row vector
for(j in 1:nrow(coef)){
  for(k in 1:nrow(coef)){
    unit_contrib_g <- rep(NA, nrow(mat)) # each unit's contribution to this jkth element of B
    for(i in 1:ncol(invlogit_coef_times_mat)){
      unit_contrib_g[i] <- mat[i,j] * mat[i,k] * 
        (x_model$y[i] - invlogit_coef_times_mat[1,i])^2 # note that x_model$y is the model outcome, which is in our model denoted x
    }
    g[j,k] <- (n_3 / n) * mean(unit_contrib_g)
  }
}

pt1 <- solve(e) %*% c %*% solve(a) %*% f %*% solve(a) %*% t(c) %*% t(solve(e)) 
pt2 <- solve(e) %*% d %*% solve(b) %*% g %*% solve(b) %*% t(d) %*% t(solve(e))
final <- pt1 + pt2
var_hat_pi_mst <- final[2,2] / n # the lower-right element divided by n is the variance estimator of interest

c(pi_hat_mst_trunc, var_hat_pi_mst)
#}
#}
