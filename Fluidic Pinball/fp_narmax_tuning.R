############################
### Tuning of parameters ###
############################
# Generate dataframe with all possible inputs
gs <- list(ny = c(3, 4),
           nu = c(3, 4),
           ne = c(3, 4),
           nl = c(3, 4),
           rho_p = c(0.00000001, 0.0000001, 0.000001, 0.00001),
           rho_e = c(0.00000001, 0.0000001, 0.000001, 0.00001))  %>% 
  cross_df() # Convert to data frame grid

# Fit model with different parameters
MSEs = rep(0, nrow(gs))

# Calculate MAE between prediction and real value for different combinations
for (i in 1:nrow(gs)) {
  
  print(i)
  
  # Create training and test set
  xu_train = as.matrix(ClCd_final[1:11400, ])
  xu_test = as.matrix(ClCd_final[11401:11700, ])
  
  model_narmax = narmax(ny = gs[i, 1]$ny, nu = gs[i, 2]$nu, ne = gs[i, 3]$ne, nl = gs[i, 4]$nl)
  model_narmax = estimate.narmax(model_narmax, xu_train[, 1:2], as.matrix(xu_train[, 3]), gs[i, 5]$rho_p, gs[i, 6]$rho_e)
  
  # Create prediction and calculate MSE
  pred_narmax = predict(model_narmax, xu_test[, 1:2], as.matrix(xu_test[, 3]), K=0)
  MSE_sum = 0
  for(j in 1:nb_series) {
    MSE = mean((xu_test[-(1:gs[i, 1]$ny), j] - pred_narmax[[j]]$yh)^2)
    MSE_sum = MSE_sum + MSE
  }
  MSEs[i] = MSE_sum/nb_series
  print(MSE_sum/nb_series)
}

MSEs_df = data.frame(index = 1:nrow(gs), gs, MSEs)

saveRDS(MSEs_df, file="Results/narmax_MSEs.RData")
