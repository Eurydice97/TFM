# Choose optimal parameters
MSEs_df = readRDS("Results/narmax_MSEs.RData")
opt_n = MSEs_df[which(MSEs_df$MSEs == min(MSEs_df$MSEs)), ]

# Initiate lists to save predictions in
preds_nn = list()
preds_n005 = list()
preds_n01 = list()
preds_n02 = list()

# Create predictions for 100 different initial points
for (i in 1:nb_trials) {
  
  print(i)
  
  # Create training and test set
  xu_train = as.matrix(ClCd_final[1:initial_points[i], ])
  xu_test = as.matrix(ClCd_final[initial_points[i]:(initial_points[i]+102), ])
  
  # Train model
  narmax_model = narmax(opt_n$ny, opt_n$nu, opt_n$ne, opt_n$nl)
  narmax_model = estimate.narmax(narmax_model, xu_train[, 1:2], as.matrix(xu_train[, 3]), opt_n$rho_p, opt_n$rho_e)
  
  ################
  ### No noise ###
  ################
  # Create predictions with no noise present
  preds_nn[[i]] = predict.narmax(narmax_model, xu_test[, 1:2], as.matrix(xu_test[, 3]), K=0)
  
  ##################
  ### 0.5% noise ###
  ##################
  # Add 0.5% of noise to initial condition
  xu_test_n005 = xu_test
  
  for(j in 1:nb_series) {
    for(k in 1:opt_n$ny) {
      xu_test_n005[k, j] = xu_test_n005[k, j] + 0.005*rnorm(1, 0, 1)
    }
  }
  
  # Create predictions
  preds_n005[[i]] = predict.narmax(narmax_model, xu_test_n005[, 1:2], as.matrix(xu_test_n005[, 3]), K=0)
  
  ################
  ### 1% noise ###
  ################
  # Add 1% of noise to initial condition
  xu_test_n01 = xu_test
  
  for(j in 1:nb_series) {
    for(k in 1:opt_n$ny) {
      xu_test_n01[k, j] = xu_test_n01[k, j] + 0.01*rnorm(1, 0, 1)
    }
  }
  
  # Create predictions
  preds_n01[[i]] = predict.narmax(narmax_model, xu_test_n01[, 1:2], as.matrix(xu_test_n01[, 3]), K=0)
  
  ################
  ### 2% noise ###
  ################
  # Add 2% of noise to initial condition
  xu_test_n02 = xu_test
  
  for(j in 1:nb_series) {
    for(k in 1:opt_n$ny) {
      xu_test_n02[k, j] = xu_test_n02[k, j] + 0.02*rnorm(1, 0, 1)
    }
  }
  
  # Create predictions
  preds_n02[[i]] = predict.narmax(narmax_model, xu_test_n02[, 1:2], as.matrix(xu_test_n02[, 3]), K=0)
}


#######################################
### Save prediction and model lists ###
#######################################
saveRDS(preds_nn, file="Results/narmax_preds_nn.RData")
saveRDS(preds_n005, file="Results/narmax_preds_n005.RData")
saveRDS(preds_n01, file="Results/narmax_preds_n01.RData")
saveRDS(preds_n02, file="Results/narmax_preds_n02.RData")
