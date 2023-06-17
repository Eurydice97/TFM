# Choose optimal parameters
MSEs_df = readRDS("Results/narmax_MSEs.RData")
opt_n = MSEs_df[which(MSEs_df$MSEs == min(MSEs_df$MSEs)), ]
MSEs_df = MSEs_df[MSEs_df$MSEs != 0, ]
opt_ns = MSEs_df[which(MSEs_df$MSEs == min(MSEs_df$MSEs)), ]
opt_n = opt_ns[nrow(opt_ns), ] # There are two combinations with the same MSE value, take the simplest on
max_lag = opt_n$ny

# Initiate lists to save predictions in
preds_nn = list()
preds_n005 = list()
preds_n01 = list()
preds_n02 = list()

# Create predictions for 100 different initial points
for (i in 1:nb_trials) {
  
  print(i)
  
  # Create training set
  if (initial_points[i] == 1) {
    train_set = as.matrix(xyzu_train)
  }
  if (initial_points[i] > 1) {
    train_set = as.matrix(rbind(xyzu_train, xyzu_pred[1:(initial_points[i]-1), ]))
  }
  
  # Create prediction set
  pred_set = as.matrix(xyzu_pred[initial_points[i]:(initial_points[i]+99+max_lag), ])
  
  # Train model
  narmax_model = narmax(opt_n$ny, opt_n$nu, opt_n$ne, opt_n$nl)
  narmax_model = estimate.narmax(narmax_model, train_set[, 1:3], as.matrix(train_set[, 4]), 1e-9, 1e-9)
  
  ################
  ### No noise ###
  ################
  # Create predictions with no noise present
  preds_nn[[i]] = predict.narmax(narmax_model, pred_set[, 1:3], as.matrix(pred_set[, 4]), K=0)
  
  ##################
  ### 0.5% noise ###
  ##################
  # Add 0.5% of noise to initial condition
  xu_test_n005 = pred_set
  
  for(j in 1:nb_series) {
    for(k in 1:opt_n$ny) {
      xu_test_n005[k, j] = xu_test_n005[k, j] + 0.005*rnorm(1, 0, 1)
    }
  }
  
  # Create predictions
  preds_n005_aux = predict.narmax(narmax_model, xu_test_n005[, 1:3], as.matrix(xu_test_n005[, 4]), K=0)
  
  # Smoothing the prediction
  for(j in 1:nb_series) {
    pred = data.frame(pred = preds_n005_aux[[j]]$yh, time = seq(0.001, 0.1, 0.001))
    smoothed_pred = np.est(data = as.matrix(pred), h.seq = 0.08, newt = NULL, estimator = "LLP", kernel = "triweight")
    preds_n005_aux[[j]]$yh_smooth = smoothed_pred
  }
  
  preds_n005[[i]] = preds_n005_aux
  
  ################
  ### 1% noise ###
  ################
  # Add 1% of noise to initial condition
  xu_test_n01 = pred_set
  
  for(j in 1:nb_series) {
    for(k in 1:opt_n$ny) {
      xu_test_n01[k, j] = xu_test_n01[k, j] + 0.01*rnorm(1, 0, 1)
    }
  }
  
  # Create predictions
  preds_n01_aux = predict.narmax(narmax_model, xu_test_n01[, 1:3], as.matrix(xu_test_n01[, 4]), K=0)
  
  # Smoothing the prediction
  for(j in 1:nb_series) {
    pred = data.frame(pred = preds_n01_aux[[j]]$yh, time = seq(0.001, 0.1, 0.001))
    smoothed_pred = np.est(data = as.matrix(pred), h.seq = 0.08, newt = NULL, estimator = "LLP", kernel = "triweight")
    preds_n01_aux[[j]]$yh_smooth = smoothed_pred
  }
  
  preds_n01[[i]] = preds_n01_aux
  
  ################
  ### 2% noise ###
  ################
  # Add 2% of noise to initial condition
  xu_test_n02 = pred_set
  
  for(j in 1:nb_series) {
    for(k in 1:opt_n$ny) {
      xu_test_n02[k, j] = xu_test_n02[k, j] + 0.02*rnorm(1, 0, 1)
    }
  }
  
  # Create predictions
  preds_n02_aux = predict.narmax(narmax_model, xu_test_n02[, 1:3], as.matrix(xu_test_n02[, 4]), K=0)
  
  # Smoothing the prediction
  for(j in 1:nb_series) {
    pred = data.frame(pred = preds_n02_aux[[j]]$yh, time = seq(0.001, 0.1, 0.001))
    smoothed_pred = np.est(data = as.matrix(pred), h.seq = 0.08, newt = NULL, estimator = "LLP", kernel = "triweight")
    preds_n02_aux[[j]]$yh_smooth = smoothed_pred
  }
  
  preds_n02[[i]] = preds_n02_aux
}


#######################################
### Save prediction and model lists ###
#######################################
saveRDS(preds_nn, file="Results/narmax_preds_nn.RData")
saveRDS(preds_n005, file="Results/narmax_preds_n005.RData")
saveRDS(preds_n01, file="Results/narmax_preds_n01.RData")
saveRDS(preds_n02, file="Results/narmax_preds_n02.RData")
