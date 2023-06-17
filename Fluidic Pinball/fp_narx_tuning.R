############################
### Tuning of parameters ###
############################
# Generate dataframe with all possible inputs
gs <- list(ny = c(3, 4),
           nu = c(3, 4),
           nl = c(3, 4),
           rho1 = c(0.000001, 0.00001, 0.0001, 0.001),
           rho2 = c(0.0000000001, 0.000000001, 0.00000001, 0.0000001))  %>% 
  cross_df() # Convert to data frame grid

# Fit model with different parameters
MSEs = rep(0, nrow(gs))

# Calculate MAE between prediction and real value for different combinations
for (i in 1:nrow(gs)) {
  
  print(i)
  
  # Create training and test set
  xu_train = as.matrix(ClCd_final[1:11400, ])
  xu_test = as.matrix(ClCd_final[11401:11700, ])
  
  # Train model
  model_narx = narx(ny = gs[i, 1]$ny, nu = gs[i, 2]$nu, nl = gs[i, 3]$nl)
  tryCatch({model_narx = estimate.narx(model_narx, xu_train[, 1:2], as.matrix(xu_train[, 3]), c(gs[i, 4]$rho1, gs[i, 5]$rho2))
  
  # Create predictions and calculate MSE
  pred_narx = predict(model_narx, xu_test[, 1:2], as.matrix(xu_test[, 3]), K=0)
  MSE_sum = 0
  for(j in 1:nb_series) {
    MSE = mean((xu_test[-(1:gs[i, 1]$ny), j] - pred_narx[[j]]$yh)^2)
    print(MSE)
    MSE_sum = MSE_sum + MSE
  }
  MSEs[i] = MSE_sum/nb_series
  print(MSE_sum/nb_series)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Choose combination that has the lowest MAE
MSEs_df = data.frame(index = 1:nrow(gs), gs, MSEs)
saveRDS(MSEs_df, file="Results/narx_MSEs.RData")


######################################
### Tuning of smoothing parameters ###
######################################
# Get optimal parameters
MSEs_df = readRDS("Results/narx_MSEs.RData")
MSEs_df = MSEs_df[MSEs_df$MSEs != 0, ]
opt_ns = MSEs_df[which(MSEs_df$MSEs == min(MSEs_df$MSEs)), ]
opt_n = opt_ns[nrow(opt_ns), ] # There are two combinations with the same MSE value, take the simplest one
max_lag = opt_n$ny

#################################################################################
# Tuning smoothing parameters using PLR
MSEs_aux_n005 = rep(0, nb_series)
MSEs_aux_n01 = rep(0, nb_series)
MSEs_aux_n02 = rep(0, nb_series)

# Create training and test sets
train_set = as.matrix(xyzu_train[1:10097, ])
val_set = as.matrix(xyzu_train[10098:nrow(xyzu_train), ])

# Train model
narx_model = narx(opt_n$ny, opt_n$nu, opt_n$nl)
narx_model = estimate.narx(narx_model, train_set[, 1:nb_series], as.matrix(train_set[, (nb_series+1)]), c(opt_n$rho1, opt_n$rho2))


##################
### 0.5% noise ###
##################
# Add 0.5% of noise to initial condition
val_set_n005 = val_set

for(j in 1:nb_series) {
  for(f in 1:opt_n$ny) {
    val_set_n005[f, j] = val_set_n005[f, j] + 0.005*rnorm(1, 0, 1)
  }
}

# Create predictions
pred_n005_aux = predict.narx(narx_model, val_set_n005[, 1:nb_series], as.matrix(val_set_n005[, (nb_series+1)]), K=0)

for(j in 1:nb_series) {
  
  pred = data.frame(pred = pred_n005_aux[[j]]$yh, time = seq(0.01, 3, 0.01))
  
  aux = np.cv(data = as.matrix(pred), h.seq = NULL, num.h = 1000, w = NULL, num.ln = 1,
              ln.0 = 0, step.ln = 2, estimator = "LLP", kernel = "triweight")
  
  MSEs_aux_n005[j] = aux$h.opt[2, ]
}


################
### 1% noise ###
################
# Add 1% of noise to initial condition
val_set_n01 = val_set

for(j in 1:nb_series) {
  for(f in 1:opt_n$ny) {
    val_set_n01[f, j] = val_set_n01[f, j] + 0.01*rnorm(1, 0, 1)
  }
}

# Create predictions
pred_n01_aux = predict.narx(narx_model, val_set_n01[, 1:nb_series], as.matrix(val_set_n01[, (nb_series+1)]), K=0)

for(j in 1:nb_series) {
  
  pred = data.frame(pred = pred_n01_aux[[j]]$yh, time = seq(0.01, 3, 0.01))
  
  aux = np.cv(data = as.matrix(pred), h.seq = NULL, num.h = 1000, w = NULL, num.ln = 1,
              ln.0 = 0, step.ln = 2, estimator = "LLP", kernel = "triweight")
  
  MSEs_aux_n01[j] = aux$h.opt[2, ]
}


################
### 2% noise ###
################
# Add 2% of noise to initial condition
val_set_n02 = val_set

for(j in 1:nb_series) {
  for(f in 1:opt_n$ny) {
    val_set_n02[f, j] = val_set_n02[f, j] + 0.02*rnorm(1, 0, 1)
  }
}

# Create predictions
pred_n02_aux = predict.narx(narx_model, val_set_n02[, 1:nb_series], as.matrix(val_set_n02[, (nb_series+1)]), K=0)

for(j in 1:nb_series) {
  
  pred = data.frame(pred = pred_n02_aux[[j]]$yh, time = seq(0.01, 3, 0.01))
  
  aux = np.cv(data = as.matrix(pred), h.seq = NULL, num.h = 1000, w = NULL, num.ln = 1,
              ln.0 = 0, step.ln = 2, estimator = "LLP", kernel = "triweight")
  print(aux$h.opt[2, ])

  MSEs_aux_n02[j] = aux$h.opt[2, ]
}

