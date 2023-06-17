###############################
### Feature matrix creation ###
###############################
############################
### Tuning of parameters ###
############################
# Potential lambdas
#lambdas = seq(0.01, 0.1, 0.001)
#lambdas = seq(0.00001, 0.01, 0.0001)
#ambdas = seq(0.000001, 0.0001, 0.000001)
#lambdas = seq(0.0000001, 0.000001, 0.00000001)

gs <- list(degree = c(3, 4, 5),
           lambda = seq(0.001, 0.05, 0.0005))  %>% 
  cross_df() # Convert to data frame grid

# Fit model with different parameters
MSEs = rep(0, nrow(gs))

#MSEs = rep(0, length(lambdas))

for (i in 1:nrow(gs)) {
  
  print(i)
  
  # Create train and test set 
  xu_train = as.matrix(ClCd_final[1:11400, ])
  xu_test = as.matrix(ClCd_final[11401:11700, ])
  x0 = xu_test[1, 1:2]                # initial point
  u = xu_test[, 3]
  steps = nrow(xu_test)
  
  # Train model 
  xu_terms_train = xu_train
  #colnames(xu) = c("Cl", "Cd", "u")
  col_names = c("Cl", "Cd", "u")
  colnames(xu_terms_train) = col_names
  
  # Add sinus of each term as feature
  for(j in 1:ncol(xu_train)) {
    xu_terms_train = cbind(xu_terms_train, sin(xu_train[, j]))
    col_name = paste0('sin(', colnames(xu_train)[j], ')')
    col_names = c(col_names, col_name)
  }
  
  # Add cosinus of each term as feature
  for(j in 1:ncol(xu_train)) {
    xu_terms_train = cbind(xu_terms_train, cos(xu_train[, j]))
    col_name = paste0('cos(', colnames(xu_train)[j], ')')
    col_names = c(col_names, col_name)
  }
  
  colnames(xu_terms_train) = col_names
  
  feat_mat_train = sindyr::features(xu_terms_train, polyorder = gs$degree[i])
  #feat_mat_train = sindyr::features(xu_train, polyorder = 3)
  sindy_model = sindy(xu_train, Theta = feat_mat_train, lambda = gs$lambda[i], dt = dt)
  coeff = data.frame(sindy_model$B)
  
  # Prediction function
  sindy_pred = function(x, u, coeff) {
    
    #xu = data.frame(t(x), u)
    #col_names(xu) = c("Cl", "Cd", "u")
    
    xu = data.frame(t(x), u)
    xu_terms_pred = xu
    col_names = c("Cl", "Cd", "u")
    colnames(xu_terms_pred) = col_names
    
    # Add sinus of each term as feature
    for(j in 1:ncol(xu)) {
      xu_terms_pred = cbind(xu_terms_pred, sin(xu[, j]))
      col_name = paste0('sin(', colnames(xu)[j], ')')
      col_names = c(col_names, col_name)
    }
    
    # Add cosinus of each term as feature
    for(j in 1:ncol(xu)) {
      xu_terms_pred = cbind(xu_terms_pred, cos(xu[, j]))
      col_name = paste0('cos(', colnames(xu)[j], ')')
      col_names = c(col_names, col_name)
    }
    
    colnames(xu_terms_pred) = col_names
  
    # Creates data matrix with terms up to degree 3
    features = sindyr::features(xu_terms_pred, polyorder = gs$degree[i])

    pred = c(features %*% coeff[, 1],
             features %*% coeff[, 2])

    return(pred)
  }
  
  # Create prediction an real values
  xhat = matrix(0, steps, nb_series)
  xhat[1, ] = x0  
  
  for (j in 1:(steps-1)) {
    xhat[j+1, ] = rk4u(sindy_pred, xhat[j, ], u[j], dt, coeff)
  }
  
  # Calculate MSE
  MSE_sum = 0
  
  for(j in 1:nb_series) {
    MSE = mean((xu_test[1:steps, j] - xhat[, j])^2)
    MSE_sum = MSE_sum + MSE
  }
  print(MSE_sum/nb_series)
  MSEs[i] = MSE_sum/nb_series
  
}

MSEs_df = data.frame(index = 1:nrow(gs), gs, MSEs)
saveRDS(MSEs_df, file="Results/sindy_MSE.RData")
