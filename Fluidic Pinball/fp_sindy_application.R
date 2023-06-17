# Choose optimal parameters
MSEs_df = readRDS("Results/sindy_MSE.RData")
opt_n = MSEs_df[which(MSEs_df$MSEs == min(MSEs_df$MSEs)), ]

# Initiate lists to save predictions in
preds_nn = list()
preds_n005 = list()
preds_n01 = list()
preds_n02 = list()

# Create predictions for n different initial points
for (i in 1:nb_trials) {
  
  print(i)
  
  # Create training and test set
  xu_train = as.matrix(ClCd_final[1:initial_points[i], ])
  xu_test = as.matrix(ClCd_final[initial_points[i]:(initial_points[i]+99), ])
  x0 = xu_test[1, 1:2]                # initial point
  u = xu_test[, 3]
  steps = 100                         # number of time steps to take
  
  # Add cos, sin to matrix of terms 
  #xu_terms_train = xu_train
  col_names = c("Cl", "Cd", "u")
  colnames(xu_train) = col_names
  #colnames(xu_terms_train) = col_names
  
  # Add sinus of each term as feature
  #for(j in 1:ncol(xu_train)) {
  #  xu_terms_train = cbind(xu_terms_train, sin(xu_train[, j]))
  #  col_name = paste0('sin(', colnames(xu_train)[j], ')')
  #  col_names = c(col_names, col_name)
  #}
  
  # Add cosinus of each term as feature
  #for(j in 1:ncol(xu_train)) {
  #  xu_terms_train = cbind(xu_terms_train, cos(xu_train[, j]))
  #  col_name = paste0('cos(', colnames(xu_train)[j], ')')
  #  col_names = c(col_names, col_name)
  #}
  
  #colnames(xu_terms_train) = col_names
  
  # Creates data matrix with terms up to degree 3
  #feat_mat_train = sindyr::features(xu_terms_train, polyorder = opt_n$degree)
  feat_mat_train = sindyr::features(xu_train, polyorder = opt_n$degree)
  
  # Train model
  #sindy_model = sindy(as.matrix(xu_terms_train), dt = dt, Theta = feat_mat_train, lambda = opt_n$lambda)
  sindy_model = sindy(xu_train, dt = dt, Theta = feat_mat_train, lambda = opt_n$lambda)
  coeff = data.frame(sindy_model$B)
  
  # Prediction function
  #sindy_pred = function(x, u, coeff) {
    
  #  xu = data.frame(t(x), u)
  #  xu_terms_pred = xu
  #  col_names = c("Cl", "Cd", "u")
  #  colnames(xu_terms_pred) = col_names
    
    # Add sinus of each term as feature
    #for(j in 1:ncol(xu)) {
    #  xu_terms_pred = cbind(xu_terms_pred, sin(xu[, j]))
    #  col_name = paste0('sin(', colnames(xu)[j], ')')
    #  col_names = c(col_names, col_name)
    #}
    
    # Add cosinus of each term as feature
    #for(j in 1:ncol(xu)) {
    #  xu_terms_pred = cbind(xu_terms_pred, cos(xu[, j]))
    #  col_name = paste0('cos(', colnames(xu)[j], ')')
    #  col_names = c(col_names, col_name)
    #}
    
    #colnames(xu_terms_pred) = col_names
    
    # Creates data matrix with terms up to degree 3
  #  features = sindyr::features(xu_terms_pred, polyorder = opt_n$degree)
    
  #  pred = c(features %*% coeff[, 1],
  #           features %*% coeff[, 2])
  #  
  #  return(pred)
  #}
  
  ################
  ### No noise ###
  ################
  # Create prediction an real values
  xhat_nn = matrix(0, steps, nb_series)
  xhat_nn[1, ] = x0  
  
  for (j in 1:(steps-1)) {
    xhat_nn[j+1, ] = rk4u(sindy_pred, xhat_nn[j, ], u[j], dt, coeff)
  }
  
  pred_aux_nn = list()
  
  for(k in 1:nb_series) {
    pred_aux_nn[[k]] = cbind(xhat_nn[, k], xu_test[, k])
    colnames(pred_aux_nn[[k]]) = c('pred', 'real')
  }
  preds_nn[[i]] = pred_aux_nn
  
  ##################
  ### 0.5% noise ###
  ##################
  # Add 0.5% of noise to initial point
  x0n005 = rep(0, length(x0))
  
  for (j in 1:nb_series) {
    x0n005[j] = x0[j] + 0.1*rnorm(1, 0, 1)
  }
  
  # Create prediction an real values
  xhat_n005 = matrix(0, steps, nb_series)
  xhat_n005[1, ] = x0n005 
  
  for (j in 1:(steps-1)) {
    xhat_n005[j+1, ] = rk4u(sindy_pred, xhat_n005[j, ], u[j], dt, coeff)
  }
  
  # Creation of final dataframes
  pred_aux_n005 = list()
  
  for(k in 1:nb_series) {
    pred_aux_n005[[k]] = cbind(xhat_n005[, k], xu_test[, k])
    colnames(pred_aux_n005[[k]]) = c('pred', 'real')
  }
  preds_n005[[i]] = pred_aux_n005
  
  ################
  ### 1% noise ###
  ################
  # Add 1% of noise to initial point
  x0n01 = rep(0, length(x0))
  
  for (j in 1:nb_series) {
    x0n01[j] = x0[j] + 0.15*rnorm(1, 0, 1)
  }
  
  # Create prediction an real values
  xhat_n01 = matrix(0, steps, nb_series)
  xhat_n01[1, ] = x0n01 
  
  for (j in 1:(steps-1)) {
    xhat_n01[j+1, ] = rk4u(sindy_pred, xhat_n01[j, ], u[j], dt, coeff)
  }
  
  # Creation of final dataframes
  pred_aux_n01 = list()
  
  for(k in 1:nb_series) {
    pred_aux_n01[[k]] = cbind(xhat_n01[, k], xu_test[, k])
    colnames(pred_aux_n01[[k]]) = c('pred', 'real')
  }
  preds_n01[[i]] = pred_aux_n01
  
  ################
  ### 2% noise ###
  ################
  # Add 2% of noise to initial point
  x0n02 = rep(0, length(x0))
  
  for (j in 1:nb_series) {
    x0n02[j] = x0[j] + 0.2*rnorm(1, 0, 1)
  }
  
  # Create prediction an real values
  xhat_n02 = matrix(0, steps, nb_series)
  xhat_n02[1, ] = x0n02 
  
  for (j in 1:(steps-1)) {
    xhat_n02[j+1, ] = rk4u(sindy_pred, xhat_n02[j, ], u[j], dt, coeff)
  }
  
  # Creation of final dataframes
  pred_aux_n02 = list()
  
  for(k in 1:nb_series) {
    pred_aux_n02[[k]] = cbind(xhat_n02[, k], xu_test[, k])
    colnames(pred_aux_n02[[k]]) = c('pred', 'real')
  }
  preds_n02[[i]] = pred_aux_n02
}


#######################################
### Save prediction and model lists ###
#######################################
saveRDS(preds_nn, file="Results/sindy_preds_nn.RData")
saveRDS(preds_n005, file="Results/sindy_preds_n005.RData")
saveRDS(preds_n01, file="Results/sindy_preds_n01.RData")
saveRDS(preds_n02, file="Results/sindy_preds_n02.RData")

11803 5.156888 5.156888
11804 5.156342 5.152513
11805 5.155802 5.147546
11806 5.155268 5.142124

11803 5.147427 5.156888
11804 5.145585 5.152513
11805 5.143769 5.147546
11806 5.141977 5.142124

11803 4.990850 5.156888
11804 4.990984 5.152513
11805 4.991133 5.147546
11806 4.991295 5.142124

11803 4.651707 5.156888
11804 4.653023 5.152513
11805 4.654597 5.147546
11806 4.656409 5.142124
