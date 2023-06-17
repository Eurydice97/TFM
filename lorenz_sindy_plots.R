#####################################
### Read in predictions and model ###
#####################################
preds_nn = readRDS("Results/sindy_preds_nn.RData")
preds_n005 = readRDS("Results/sindy_preds_n005.RData")
preds_n01 = readRDS("Results/sindy_preds_n01.RData")
preds_n02 = readRDS("Results/sindy_preds_n02.RData")
MSEs_df = readRDS("Results/sindy_MSEs.RData")
nb_time_steps = nrow(preds_nn[[1]][[1]])
coordinates = c('x', 'y', 'z')


#################################
### Plots of no noise results ###
#################################
# Calculate prediction error
NRMSEs_nn = matrix(0, nb_trials, nb_series)

for(i in 1:nb_trials) {
  for(j in 1:nb_series) {
    real = data.frame(preds_nn[[i]][[j]])$real
    pred = data.frame(preds_nn[[i]][[j]])$pred
    NRMSE = nrmse(pred, real, na.rm=TRUE, norm="maxmin")
    NRMSEs_nn[i, j] = NRMSE
  }
}

NRMSEs_nn = data.frame(NRMSEs_nn)
NRMSEs_nn$index = 1:nb_trials
names(NRMSEs_nn) = c('x', 'y', 'z', 'index')

# Calculation of max, min, mean, var
mins_nn = apply(NRMSEs_nn, 2, min)   
means_nn = apply(NRMSEs_nn, 2, mean) 
vars_nn = apply(NRMSEs_nn, 2, var)   
maxs_nn = apply(NRMSEs_nn, 2, max)

# Calculate average NRMSE and for each time step of each 
NRMSEs_nn_timestep = matrix(0, nb_time_steps, nb_series)
NRMSEs_nn_timestep_minCI = matrix(0, nb_time_steps, nb_series)
NRMSEs_nn_timestep_maxCI = matrix(0, nb_time_steps, nb_series)

# Calculate NRMSE and CI for each time series component
for(j in 1:nb_series) {
  
  # Calculate prediction error for each time step for all trials
  for(i in 1:nb_time_steps) {
    pred_aux = matrix(0, nb_trials, 2)
    for(k in 1:nb_trials) {
      pred_aux[k, 1] = data.frame(preds_nn[[k]][[j]])$real[i]
      pred_aux[k, 2] = data.frame(preds_nn[[k]][[j]])$pred[i]
    }
    
    # Calculate NRMSE for each timestep
    obs_minmax = max(pred_aux[, 1]) - min(pred_aux[, 1])
    pred_errors = (pred_aux[, 1] - pred_aux[, 2])^2
    
    NRMSE = sqrt(mean(pred_errors))/obs_minmax
    NRMSEs_nn_timestep[i, j] = NRMSE
    
    # Apply non parametric bootstrap of NRMSE statistic
    boot_out = boot::boot(pred_aux, R = 1000, statistic = NRMSE_function)
    CI = boot.ci(boot_out, type = "basic")
    NRMSEs_nn_timestep_minCI[i, j] = CI$basic[4]
    NRMSEs_nn_timestep_maxCI[i, j] = CI$basic[5]
  }
}

# Plot results of complete dataset and prediction
for(j in 1:(nb_series)) {
  
  df_real = data.frame(time = seq(1,100,length=100), Coordinate = data.frame(preds_nn[[1]][[j]])$real[1:100],
                       Label = "Real")
  df_pred = data.frame(time = seq(1,100,length=100), Coordinate = data.frame(preds_nn[[1]][[j]])$pred[1:100],
                       Label = "Predicted")
  
  df = rbind(df_real, df_pred)
  
  print(ggplot(df, aes(x = time, y = Coordinate, color = Label)) + 
          geom_line(size=1.5) +
          theme_minimal() +
          theme(plot.title = element_text(size = 30,face="bold"), 
                axis.text=element_text(size=25),
                axis.title=element_text(size=30,face="bold"), 
                legend.title = element_text(size = 30,face="bold"),
                legend.text = element_text(size=25)))
}


######################################
### Plots of results of 0.5% noise ###
######################################
# Calculate prediction error
NRMSEs_n005 = matrix(0, nb_trials, nb_series)

for(i in 1:nb_trials) {
  for(j in 1:nb_series) {
    real = data.frame(preds_n005[[i]][[j]])$real
    pred = data.frame(preds_n005[[i]][[j]])$pred
    NRMSE = nrmse(pred, real, na.rm=TRUE, norm="maxmin")
    NRMSEs_n005[i, j] = NRMSE
  }
}

NRMSEs_n005 = data.frame(NRMSEs_n005)
NRMSEs_n005$index = 1:nb_trials
names(NRMSEs_n005) = c('x', 'y', 'z', 'index')

# Calculation of max, min, mean, var
mins_n005 = apply(NRMSEs_n005, 2, min)   
means_n005 = apply(NRMSEs_n005, 2, mean) 
vars_n005 = apply(NRMSEs_n005, 2, var)   
maxs_n005 = apply(NRMSEs_n005, 2, max)  


# Calculate prediction error
NRMSEs_n005_smooth = matrix(0, nb_trials, nb_series)

for(i in 1:nb_trials) {
  for(j in 1:nb_series) {
    real = preds_n005[[i]][[j]]$real
    pred = as.numeric(preds_n005[[i]][[j]]$pred_smooth)
    NRMSE = nrmse(pred, real, na.rm=TRUE, norm="maxmin")
    NRMSEs_n005_smooth[i, j] = NRMSE
  }
}

NRMSEs_n005_smooth = data.frame(NRMSEs_n005_smooth)
NRMSEs_n005_smooth$index = 1:nb_trials
names(NRMSEs_n005_smooth) = c('x', 'y', 'z', 'index')

# Calculation of max, min, mean, var
mins_n005_smooth = apply(NRMSEs_n005_smooth, 2, min)   
means_n005_smooth = apply(NRMSEs_n005_smooth, 2, mean) 
vars_n005_smooth = apply(NRMSEs_n005_smooth, 2, var)   
maxs_n005_smooth = apply(NRMSEs_n005_smooth, 2, max)  


# Calculate average NRMSE and for each time step of each 
NRMSEs_n005_timestep = matrix(0, nb_time_steps, nb_series)
NRMSEs_n005_timestep_minCI = matrix(0, nb_time_steps, nb_series)
NRMSEs_n005_timestep_maxCI = matrix(0, nb_time_steps, nb_series)

# Calculate NRMSE and CI for each time series component
for(j in 1:nb_series) {
  
  # Calculate prediction error for each time step for all trials
  for(i in 1:nb_time_steps) {
    pred_aux = matrix(0, nb_trials, 2)
    for(k in 1:nb_trials) {
      pred_aux[k, 1] = data.frame(preds_n005[[k]][[j]])$real[i]
      pred_aux[k, 2] = data.frame(preds_n005[[k]][[j]])$pred[i]
    }
    
    # Calculate NRMSE for each timestep
    obs_minmax = max(pred_aux[, 1]) - min(pred_aux[, 1])
    pred_errors = (pred_aux[, 1] - pred_aux[, 2])^2
    
    NRMSE = sqrt(mean(pred_errors))/obs_minmax
    NRMSEs_n005_timestep[i, j] = NRMSE
    
    # Apply non parametric bootstrap of NRMSE statistic
    boot_out = boot::boot(pred_aux, R = 1000, statistic = NRMSE_function)
    CI = boot.ci(boot_out, type = "basic")
    NRMSEs_n005_timestep_minCI[i, j] = CI$basic[4]
    NRMSEs_n005_timestep_maxCI[i, j] = CI$basic[5]
  }
}

# Calculate average NRMSE and for each time step of each 
NRMSEs_n005_timestep_smooth = matrix(0, nb_time_steps, nb_series)
NRMSEs_n005_timestep_minCI_smooth = matrix(0, nb_time_steps, nb_series)
NRMSEs_n005_timestep_maxCI_smooth = matrix(0, nb_time_steps, nb_series)

# Calculate NRMSE and CI for each time series component
for(j in 1:nb_series) {
  
  # Calculate prediction error for each time step for all trials
  for(i in 1:nb_time_steps) {
    pred_aux = matrix(0, nb_trials, 2)
    for(k in 1:nb_trials) {
      pred_aux[k, 1] = data.frame(preds_n005[[k]][[j]])$real[i]
      pred_aux[k, 2] = data.frame(preds_n005[[k]][[j]])$pred_smooth[i]
    }
    
    # Calculate NRMSE for each timestep
    obs_minmax = max(pred_aux[, 1]) - min(pred_aux[, 1])
    pred_errors = (pred_aux[, 1] - pred_aux[, 2])^2
    
    NRMSE = sqrt(mean(pred_errors))/obs_minmax
    NRMSEs_n005_timestep_smooth[i, j] = NRMSE
    
    # Apply non parametric bootstrap of NRMSE statistic
    boot_out = boot::boot(pred_aux, R = 1000, statistic = NRMSE_function)
    CI = boot.ci(boot_out, type = "basic")
    NRMSEs_n005_timestep_minCI_smooth[i, j] = CI$basic[4]
    NRMSEs_n005_timestep_maxCI_smooth[i, j] = CI$basic[5]
  }
}


# Plot results of complete dataset and prediction
for(j in 1:(nb_series)) {
  
  df_real = data.frame(time = seq(1,100,length=100), Coordinate = data.frame(preds_n005[[1]][[j]])$real[1:100],
                       Label = "Real")
  df_pred = data.frame(time = seq(1,100,length=100), Coordinate = data.frame(preds_n005[[1]][[j]])$pred[1:100],
                       Label = "Predicted")
  
  df = rbind(df_real, df_pred)
  
  print(ggplot(df, aes(x = time, y = Coordinate, color = Label)) + 
          geom_line(size=1.5) +
          theme_minimal() +
          theme(plot.title = element_text(size = 30,face="bold"), 
                axis.text=element_text(size=25),
                axis.title=element_text(size=30,face="bold"), 
                legend.title = element_text(size = 30,face="bold"),
                legend.text = element_text(size=25)))
}


####################################
### Plots of results of 1% noise ###
####################################
# Calculate prediction error
NRMSEs_n01 = matrix(0, nb_trials, nb_series)

for(i in 1:nb_trials) {
  for(j in 1:nb_series) {
    real = preds_n01[[i]][[j]]$real
    pred = preds_n01[[i]][[j]]$pred
    NRMSE = nrmse(pred, real, na.rm=TRUE, norm="maxmin")
    NRMSEs_n01[i, j] = NRMSE
  }
}

NRMSEs_n01 = data.frame(NRMSEs_n01)
NRMSEs_n01$total = rowSums(NRMSEs_n01)
NRMSEs_n01$index = 1:nb_trials
names(NRMSEs_n01) = c('x', 'y', 'z', 'total', 'index')
NRMSEs_n01_long = gather(NRMSEs_n01, key="variable", value="NRMSEs", 1:2)

# Calculation of max, min, mean, var
mins_n01 = apply(NRMSEs_n01, 2, min)   
means_n01 = apply(NRMSEs_n01, 2, mean) 
vars_n01 = apply(NRMSEs_n01, 2, var)   
maxs_n01 = apply(NRMSEs_n01, 2, max)  


# Calculate prediction error
NRMSEs_n01_smooth = matrix(0, nb_trials, nb_series)

for(i in 1:nb_trials) {
  for(j in 1:nb_series) {
    real = preds_n01[[i]][[j]]$real
    pred = as.numeric(preds_n01[[i]][[j]]$pred_smooth)
    NRMSE = nrmse(pred, real, na.rm=TRUE, norm="maxmin")
    NRMSEs_n01_smooth[i, j] = NRMSE
  }
}

NRMSEs_n01_smooth = data.frame(NRMSEs_n01_smooth)
NRMSEs_n01_smooth$index = 1:nb_trials
names(NRMSEs_n01_smooth) = c('x', 'y', 'z', 'index')

# Calculation of max, min, mean, var
mins_n01_smooth = apply(NRMSEs_n01_smooth, 2, min)   
means_n01_smooth = apply(NRMSEs_n01_smooth, 2, mean) 
vars_n01_smooth = apply(NRMSEs_n01_smooth, 2, var)   
maxs_n01_smooth = apply(NRMSEs_n01_smooth, 2, max)  

# Calculate average NRMSE and for each time step of each 
NRMSEs_n01_timestep = matrix(0, nb_time_steps, nb_series)
NRMSEs_n01_timestep_minCI = matrix(0, nb_time_steps, nb_series)
NRMSEs_n01_timestep_maxCI = matrix(0, nb_time_steps, nb_series)

# Calculate NRMSE and CI for each time series component
for(j in 1:nb_series) {
  
  # Calculate prediction error for each time step for all trials
  for(i in 1:nb_time_steps) {
    pred_aux = matrix(0, nb_trials, 2)
    for(k in 1:nb_trials) {
      pred_aux[k, 1] = data.frame(preds_n01[[k]][[j]])$real[i]
      pred_aux[k, 2] = data.frame(preds_n01[[k]][[j]])$pred[i]
    }
    
    # Calculate NRMSE for each timestep
    obs_minmax = max(pred_aux[, 1]) - min(pred_aux[, 1])
    pred_errors = (pred_aux[, 1] - pred_aux[, 2])^2
    
    NRMSE = sqrt(mean(pred_errors))/obs_minmax
    NRMSEs_n01_timestep[i, j] = NRMSE
    
    # Apply non parametric bootstrap of NRMSE statistic
    boot_out = boot::boot(pred_aux, R = 1000, statistic = NRMSE_function)
    CI = boot.ci(boot_out, type = "basic")
    NRMSEs_n01_timestep_minCI[i, j] = CI$basic[4]
    NRMSEs_n01_timestep_maxCI[i, j] = CI$basic[5]
  }
}

# Calculate average NRMSE and for each time step of each 
NRMSEs_n01_timestep_smooth = matrix(0, nb_time_steps, nb_series)
NRMSEs_n01_timestep_minCI_smooth = matrix(0, nb_time_steps, nb_series)
NRMSEs_n01_timestep_maxCI_smooth = matrix(0, nb_time_steps, nb_series)

# Calculate NRMSE and CI for each time series component
for(j in 1:nb_series) {
  
  # Calculate prediction error for each time step for all trials
  for(i in 1:nb_time_steps) {
    pred_aux = matrix(0, nb_trials, 2)
    for(k in 1:nb_trials) {
      pred_aux[k, 1] = data.frame(preds_n01[[k]][[j]])$real[i]
      pred_aux[k, 2] = data.frame(preds_n01[[k]][[j]])$pred_smooth[i]
    }
    
    # Calculate NRMSE for each timestep
    obs_minmax = max(pred_aux[, 1]) - min(pred_aux[, 1])
    pred_errors = (pred_aux[, 1] - pred_aux[, 2])^2
    
    NRMSE = sqrt(mean(pred_errors))/obs_minmax
    NRMSEs_n01_timestep_smooth[i, j] = NRMSE
    
    # Apply non parametric bootstrap of NRMSE statistic
    boot_out = boot::boot(pred_aux, R = 1000, statistic = NRMSE_function)
    CI = boot.ci(boot_out, type = "basic")
    NRMSEs_n01_timestep_minCI_smooth[i, j] = CI$basic[4]
    NRMSEs_n01_timestep_maxCI_smooth[i, j] = CI$basic[5]
  }
}

# Plot results of complete dataset and prediction
for(j in 1:(nb_series)) {
  
  df_real = data.frame(time = seq(1,100,length=100), Coordinate = data.frame(preds_n01[[1]][[j]])$real[1:100],
                       Label = "Real")
  df_pred = data.frame(time = seq(1,100,length=100), Coordinate = data.frame(preds_n01[[1]][[j]])$pred[1:100],
                       Label = "Predicted")
  
  df = rbind(df_real, df_pred)
  
  print(ggplot(df, aes(x = time, y = Coordinate, color = Label)) + 
          geom_line(size=1.5) +
          theme_minimal() +
          theme(plot.title = element_text(size = 30,face="bold"), 
                axis.text=element_text(size=25),
                axis.title=element_text(size=30,face="bold"), 
                legend.title = element_text(size = 30,face="bold"),
                legend.text = element_text(size=25)))
}


######################################
### Plots of results with 2% noise ###
######################################
# Calculate prediction error
NRMSEs_n02 = matrix(0, nb_trials, nb_series)

for(i in 1:nb_trials) {
  for(j in 1:nb_series) {
    real = data.frame(preds_n02[[i]][[j]])$real
    pred = data.frame(preds_n02[[i]][[j]])$pred
    NRMSE = nrmse(pred, real, na.rm=TRUE, norm="maxmin")
    NRMSEs_n02[i, j] = NRMSE
  }
}

NRMSEs_n02 = data.frame(NRMSEs_n02)
NRMSEs_n02$total = rowSums(NRMSEs_n02)
NRMSEs_n02$index = 1:nb_trials
names(NRMSEs_n02) = c('x', 'y', 'z', 'total', 'index')
NRMSEs_n02_long = gather(NRMSEs_n02, key="variable", value="NRMSEs", 1:2)

# Calculation of max, min, mean, var
mins_n02 = apply(NRMSEs_n02, 2, min)  
means_n02 = apply(NRMSEs_n02, 2, mean)
vars_n02 = apply(NRMSEs_n02, 2, var)  
maxs_n02 = apply(NRMSEs_n02, 2, max) 


# Calculate prediction error
NRMSEs_n02_smooth = matrix(0, nb_trials, nb_series)

for(i in 1:nb_trials) {
  for(j in 1:nb_series) {
    real = preds_n02[[i]][[j]]$real
    pred = as.numeric(preds_n02[[i]][[j]]$pred_smooth)
    NRMSE = nrmse(pred, real, na.rm=TRUE, norm="maxmin")
    NRMSEs_n02_smooth[i, j] = NRMSE
  }
}

NRMSEs_n02_smooth = data.frame(NRMSEs_n02_smooth)
NRMSEs_n02_smooth$index = 1:nb_trials
names(NRMSEs_n02_smooth) = c('x', 'y', 'z', 'index')

# Calculation of max, min, mean, var
mins_n02_smooth = apply(NRMSEs_n02_smooth, 2, min)   
means_n02_smooth = apply(NRMSEs_n02_smooth, 2, mean) 
vars_n02_smooth = apply(NRMSEs_n02_smooth, 2, var)   
maxs_n02_smooth = apply(NRMSEs_n02_smooth, 2, max)  


# Calculate average NRMSE and for each time step of each 
NRMSEs_n02_timestep = matrix(0, nb_time_steps, nb_series)
NRMSEs_n02_timestep_minCI = matrix(0, nb_time_steps, nb_series)
NRMSEs_n02_timestep_maxCI = matrix(0, nb_time_steps, nb_series)

# Calculate NRMSE and CI for each time series component
for(j in 1:nb_series) {
  
  # Calculate prediction error for each time step for all trials
  for(i in 1:nb_time_steps) {
    pred_aux = matrix(0, nb_trials, 2)
    for(k in 1:nb_trials) {
      pred_aux[k, 1] = data.frame(preds_n02[[k]][[j]])$real[i]
      pred_aux[k, 2] = data.frame(preds_n02[[k]][[j]])$pred[i]
    }
    
    # Calculate NRMSE for each timestep
    obs_minmax = max(pred_aux[, 1]) - min(pred_aux[, 1])
    pred_errors = (pred_aux[, 1] - pred_aux[, 2])^2
    
    NRMSE = sqrt(mean(pred_errors))/obs_minmax
    NRMSEs_n02_timestep[i, j] = NRMSE
    
    # Apply non parametric bootstrap of NRMSE statistic
    boot_out = boot::boot(pred_aux, R = 1000, statistic = NRMSE_function)
    CI = boot.ci(boot_out, type = "basic")
    NRMSEs_n02_timestep_minCI[i, j] = CI$basic[4]
    NRMSEs_n02_timestep_maxCI[i, j] = CI$basic[5]
  }
}

# Calculate average NRMSE and for each time step of each 
NRMSEs_n02_timestep_smooth = matrix(0, nb_time_steps, nb_series)
NRMSEs_n02_timestep_minCI_smooth = matrix(0, nb_time_steps, nb_series)
NRMSEs_n02_timestep_maxCI_smooth = matrix(0, nb_time_steps, nb_series)

# Calculate NRMSE and CI for each time series component
for(j in 1:nb_series) {
  
  # Calculate prediction error for each time step for all trials
  for(i in 1:nb_time_steps) {
    pred_aux = matrix(0, nb_trials, 2)
    for(k in 1:nb_trials) {
      pred_aux[k, 1] = data.frame(preds_n02[[k]][[j]])$real[i]
      pred_aux[k, 2] = data.frame(preds_n02[[k]][[j]])$pred_smooth[i]
    }
    
    # Calculate NRMSE for each timestep
    obs_minmax = max(pred_aux[, 1]) - min(pred_aux[, 1])
    pred_errors = (pred_aux[, 1] - pred_aux[, 2])^2
    
    NRMSE = sqrt(mean(pred_errors))/obs_minmax
    NRMSEs_n02_timestep_smooth[i, j] = NRMSE
    
    # Apply non parametric bootstrap of NRMSE statistic
    boot_out = boot::boot(pred_aux, R = 1000, statistic = NRMSE_function)
    CI = boot.ci(boot_out, type = "basic")
    NRMSEs_n02_timestep_minCI_smooth[i, j] = CI$basic[4]
    NRMSEs_n02_timestep_maxCI_smooth[i, j] = CI$basic[5]
  }
}

# Plot results of complete dataset and prediction
for(j in 1:(nb_series)) {
  
  df_real = data.frame(time = seq(1,100,length=100), Coordinate = data.frame(preds_n02[[1]][[j]])$real[1:100],
                       Label = "Real")
  df_pred = data.frame(time = seq(1,100,length=100), Coordinate = data.frame(preds_n02[[1]][[j]])$pred[1:100],
                       Label = "Predicted")
  
  df = rbind(df_real, df_pred)
  
  print(ggplot(df, aes(x = time, y = Coordinate, color = Label)) + 
          geom_line(size=1.5) +
          theme_minimal() +
          theme(plot.title = element_text(size = 30,face="bold"), 
                axis.text=element_text(size=25),
                axis.title=element_text(size=30,face="bold"), 
                legend.title = element_text(size = 30,face="bold"),
                legend.text = element_text(size=25)))
}


##################
### More plots ###
##################
for(i in 1:nb_series) {
  
  # Create dataframe with all NRMSE values per trial of coordinate
  NRMSEs_x = data.frame(index = 1:nb_trials, NRMSEs_nn[, i], 
                        NRMSEs_n005_smooth[, i],
                        NRMSEs_n01_smooth[, i], 
                        NRMSEs_n02_smooth[, i])
  
  names(NRMSEs_x) = c('Index', '0.0% noise', '0.5% noise', '1.0% noise', '2.0% noise')
  
  # Concatinate all NRMSE values for all levels of noise into a long df
  NRMSEs_x_long = gather(NRMSEs_x, key="Noise", value="NRMSE", 2:5)
  
  # Add vector to order noise properly
  order_vec = c(rep(4, 100), rep(3, 100), rep(2, 100), rep(1, 100))
  NRMSEs_x_long = data.frame(NRMSEs_x_long, order_vec)
  
  # Plot estimaton of the PDFs using kernel density
  #print(ggplot(NRMSEs_x_long, aes(x = NRMSE, y = fct_reorder(Noise, order_vec, .fun = min), fill = Noise)) +
  #        geom_density_ridges() +
  #        xlab("NRMSE") + ylab("Density") +
  #        theme_ridges() + 
  #        theme(plot.title = element_text(size = 45,face="bold"), 
  #              axis.text=element_text(size=40),
  #              axis.title=element_text(size=45,face="bold"), 
  #              legend.text = element_text(size=35), 
  #              legend.title = element_text(size=40), 
  #              legend.position = "none"))
  
  print(ggplot(NRMSEs_x_long, aes(x = NRMSE, fill = Noise, colour = Noise)) +
          theme_minimal() +
          theme(plot.title = element_text(size = 50,face="bold"), 
                axis.text=element_text(size=40),
                axis.title=element_text(size=45,face="bold"), 
                legend.text = element_text(size=40),
                legend.title = element_text(size = 45,face="bold")) +
          xlab("NRMSE") + ylab("Density") +
          geom_density(alpha = 0.6, aes(color=Noise, fill = Noise))
  )
  
  print(ggplot(NRMSEs_x_long, aes(x = NRMSE, fill = Noise, colour = Noise)) + 
          geom_histogram(alpha = 0.5, position = "identity", binwidth = 0.01))
  
  # Prepare NRMSE
  # Create a dataframe with all NRMSE values per time step of coordinate 
  NRMSEs_x_timestep = data.frame(index = 1:nb_time_steps, NRMSEs_nn_timestep[, i], 
                                 NRMSEs_n005_timestep_smooth[, i], 
                                 NRMSEs_n01_timestep_smooth[, i], 
                                 NRMSEs_n02_timestep_smooth[, i])
  
  names(NRMSEs_x_timestep) = c('Timestep', '0.0%', '0.5%', '1.0%', '2.0%')
  
  # Concatinate all NRMSE values for all levels of noise into a long df
  NRMSEs_x_timestep_long = gather(NRMSEs_x_timestep, key="Noise", value="NRMSE", 2:5)
  
  # Prepare NRMSE min CI
  # Create a dataframe with all min CIs of the NRMSE values per time step of coordinate 
  NRMSEs_x_timestep_minCI = data.frame(index = 1:nb_time_steps, NRMSEs_nn_timestep_minCI[, i], 
                                       NRMSEs_n005_timestep_minCI_smooth[, i], 
                                       NRMSEs_n01_timestep_minCI_smooth[, i], 
                                       NRMSEs_n02_timestep_minCI_smooth[, i])
  
  names(NRMSEs_x_timestep_minCI) = c('Timestep', '0.0%', '0.5%', '1.0%', '2.0%')
  
  # Concatinate all NRMSE values for all levels of noise into a long df
  NRMSEs_x_timestep_long_minCI = gather(NRMSEs_x_timestep_minCI, key="Noise", value="min_CI", 2:5)
  
  # Prepare NRMSE max CI
  # Create a dataframe with all max CIs of the NRMSE values per time step of coordinate 
  NRMSEs_x_timestep_maxCI = data.frame(index = 1:nb_time_steps, NRMSEs_nn_timestep_maxCI[, i], 
                                       NRMSEs_n005_timestep_maxCI_smooth[, i], 
                                       NRMSEs_n01_timestep_maxCI_smooth[, i], 
                                       NRMSEs_n02_timestep_maxCI_smooth[, i])
  
  names(NRMSEs_x_timestep_maxCI) = c('Timestep', '0.0%', '0.5%', '1.0%', '2.0%')
  
  # Concatinate all NRMSE values for all levels of noise into a long df
  NRMSEs_x_timestep_long_maxCI = gather(NRMSEs_x_timestep_maxCI, key="Noise", value="max_CI", 2:5)
  
  df_joined = data.frame(NRMSEs_x_timestep_long, 
                         "min_CI" = NRMSEs_x_timestep_long_minCI[, 3],
                         "max_CI" = NRMSEs_x_timestep_long_maxCI[, 3])
  
  df_joined$Noise  <- with(df_joined, reorder(Noise, NRMSE))
  
  # Plot the evolution of NRMSE over time
  print(ggplot(df_joined, aes(x = Timestep, y = NRMSE, group = Noise)) +
          theme_minimal() +
          theme(plot.title = element_text(size = 50,face="bold"), 
                axis.text=element_text(size=40),
                axis.title=element_text(size=45,face="bold"), 
                legend.text = element_text(size=40),
                legend.title = element_text(size = 45,face="bold")) +
          geom_line(aes(color=Noise), size = 1.7) +
          geom_point(aes(color=Noise), size = 1.7) +
          geom_ribbon(aes(y = NRMSE, ymin = min_CI, ymax = max_CI, fill = Noise), alpha = .2))
}


######################################
### Tests to compare distributions ###
######################################
# Kolmogorov-Smirnov test
ks.test(NRMSEs_nn$x, NRMSEs_n005_smooth$x)  
ks.test(NRMSEs_nn$x, NRMSEs_n01_smooth$x)  
ks.test(NRMSEs_nn$x, NRMSEs_n02_smooth$x) 

ks.test(NRMSEs_n005_smooth$x, NRMSEs_n01_smooth$x)
ks.test(NRMSEs_n005_smooth$x, NRMSEs_n02_smooth$x) 

ks.test(NRMSEs_n01_smooth$x, NRMSEs_n02_smooth$x)


ks.test(NRMSEs_nn$y, NRMSEs_n005_smooth$y)  
ks.test(NRMSEs_nn$y, NRMSEs_n01_smooth$y) 
ks.test(NRMSEs_nn$y, NRMSEs_n02_smooth$y) 

ks.test(NRMSEs_n005_smooth$y, NRMSEs_n01_smooth$y) 
ks.test(NRMSEs_n005_smooth$y, NRMSEs_n02_smooth$y) 

ks.test(NRMSEs_n01_smooth$y, NRMSEs_n02_smooth$y)


ks.test(NRMSEs_nn$z, NRMSEs_n005_smooth$z) 
ks.test(NRMSEs_nn$z, NRMSEs_n01_smooth$z) 
ks.test(NRMSEs_nn$z, NRMSEs_n02_smooth$z) 

ks.test(NRMSEs_n005_smooth$z, NRMSEs_n01_smooth$z)
ks.test(NRMSEs_n005_smooth$z, NRMSEs_n02_smooth$z)

ks.test(NRMSEs_n01_smooth$z, NRMSEs_n02_smooth$z) 

