# Choose optimal parameters
MSEs_df = readRDS("Results/narx_MSEs.RData")
opt_n = MSEs_df[which(MSEs_df$MSEs == min(MSEs_df$MSEs)), ][1,]

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
  narx_model = narx(opt_n$ny, opt_n$nu, opt_n$nl)
  narx_model = estimate.narx(narx_model, xu_train[, 1:2], as.matrix(xu_train[, 3]), c(opt_n$rho1, opt_n$rho2))
  
  ################
  ### No noise ###
  ################
  # Create predictions with no noise present
  preds_nn[[i]] = predict.narx(narx_model, xu_test[, 1:2], as.matrix(xu_test[, 3]), K=0)
  
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
  preds_n005_aux = predict.narx(narx_model, xu_test_n005[, 1:2], as.matrix(xu_test_n005[, 3]), K=0)
  
  # Smoothing the prediction
  for(j in 1:nb_series) {
    pred = data.frame(pred = preds_n005_aux[[j]]$yh, time = seq(0.01, 1, 0.01))
    smoothed_pred = np.est(data = as.matrix(pred), h.seq = MSEs_aux_n005[j], newt = NULL, estimator = "LLP", kernel = "triweight")
    preds_n005_aux[[j]]$yh_smooth = smoothed_pred
  }
  
  preds_n005[[i]] = preds_n005_aux
  
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
  preds_n01_aux = predict.narx(narx_model, xu_test_n01[, 1:2], as.matrix(xu_test_n01[, 3]), K=0)
  
  # Smoothing the prediction
  for(j in 1:nb_series) {
    pred = data.frame(pred = preds_n01_aux[[j]]$yh, time = seq(0.01, 1, 0.01))
    smoothed_pred = np.est(data = as.matrix(pred), h.seq = MSEs_aux_n01[j], newt = NULL, estimator = "LLP", kernel = "triweight")
    preds_n01_aux[[j]]$yh_smooth = smoothed_pred
  }
  
  preds_n01[[i]] = preds_n01_aux
  
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
  preds_n02_aux = predict.narx(narx_model, xu_test_n02[, 1:2], as.matrix(xu_test_n02[, 3]), K=0)
  
  # Smoothing the prediction
  for(j in 1:nb_series) {
    pred = data.frame(pred = preds_n02_aux[[j]]$yh, time = seq(0.01, 1, 0.01))
    smoothed_pred = np.est(data = as.matrix(pred), h.seq = MSEs_aux_n02[j], newt = NULL, estimator = "LLP", kernel = "triweight")
    preds_n02_aux[[j]]$yh_smooth = smoothed_pred
  }
  
  preds_n02[[i]] = preds_n02_aux
}


#######################################
### Save prediction and model lists ###
#######################################
saveRDS(preds_nn, file="Results/narx_preds_nn.RData")
saveRDS(preds_n005, file="Results/narx_preds_n005.RData")
saveRDS(preds_n01, file="Results/narx_preds_n01.RData")
saveRDS(preds_n02, file="Results/narx_preds_n02.RData")
