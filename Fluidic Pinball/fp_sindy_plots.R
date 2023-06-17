#####################################
### Read in predictions and model ###
#####################################
preds_nn = readRDS("Results/sindy_preds_nn.RData")
preds_n005 = readRDS("Results/sindy_preds_n005.RData")
preds_n01 = readRDS("Results/sindy_preds_n01.RData")
preds_n02 = readRDS("Results/sindy_preds_n02.RData")
sindy_model = readRDS("Results/sindy_model.RData")
#MSEs_df = readRDS("Results/sindy_MSEs.RData")
nb_time_steps = nrow(preds_nn[[1]][[1]])
coordinates = c('Cl', 'Cd')


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
    print(NRMSE)
    print("##################")
  }
}

NRMSEs_nn = data.frame(NRMSEs_nn)
NRMSEs_nn$total = rowSums(NRMSEs_nn)
NRMSEs_nn$index = 1:nb_trials
names(NRMSEs_nn) = c('Cl', 'Cd', 'total', 'index')
NRMSEs_nn_long = gather(NRMSEs_nn, key="variable", value="NRMSEs", 1:2)

# Calculation of max, min, mean, var
mins_nn = apply(NRMSEs_nn, 2, min)  
means_nn = apply(NRMSEs_nn, 2, mean)
vars_nn = apply(NRMSEs_nn, 2, var)  
maxs_nn = apply(NRMSEs_nn, 2, max)  

# Calculate average NRMSE and for each time step of each 
NRMSEs_nn_timestep = matrix(0, nb_time_steps, nb_series)
NRMSEs_nn_timestep_sd = matrix(0, nb_time_steps, nb_series)

for(j in 1:nb_series) {
  pred_error_aux = matrix(0, nb_time_steps, nb_trials)
  obs_aux = matrix(0, nb_time_steps, nb_trials)
  
  # Calculate prediction error for each time step for all trials
  for(i in 1:nb_time_steps) {
    for(k in 1:nb_trials) {
      pred_error_aux[i, k] = (data.frame(preds_nn[[k]][[j]])$real[i] - data.frame(preds_nn[[k]][[j]])$pred[i])^2
      obs_aux[i, k] = data.frame(preds_nn[[k]][[j]])$real[i]
    }
  }
  # Calculate max - min for each time step and use it normalization factor
  obs_minmax = apply(obs_aux, 1, max) - apply(obs_aux, 1, min)
  
  # Calculate normalized mean square error and its corresponding standard deviation and save 
  NRMSE = sqrt(apply(pred_error_aux, 1, mean))/obs_minmax
  sd_NRMSE = sqrt(apply(pred_error_aux, 1, var))/obs_minmax
  NRMSEs_nn_timestep[, j] = NRMSE
  NRMSEs_nn_timestep_sd[, j] = sd_NRMSE
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
    print(NRMSE)
  }
  print("##################")
}

NRMSEs_n005 = data.frame(NRMSEs_n005)
NRMSEs_n005$total = rowSums(NRMSEs_n005)
NRMSEs_n005$index = 1:nb_trials
names(NRMSEs_n005) = c('Cl', 'Cd', 'total', 'index')
NRMSEs_n005_long = gather(NRMSEs_n005, key="variable", value="NRMSEs", 1:2)

# Calculation of max, min, mean, var
mins_n005 = apply(NRMSEs_n005, 2, min)  
means_n005 = apply(NRMSEs_n005, 2, mean)
vars_n005 = apply(NRMSEs_n005, 2, var)  
maxs_n005 = apply(NRMSEs_n005, 2, max)  

# Calculate average NRMSE and for each time step of each 
NRMSEs_n005_timestep = matrix(0, nb_time_steps, nb_series)
NRMSEs_n005_timestep_sd = matrix(0, nb_time_steps, nb_series)

for(j in 1:nb_series) {
  pred_error_aux = matrix(0, nb_time_steps, nb_trials)
  obs_aux = matrix(0, nb_time_steps, nb_trials)
  
  # Calculate prediction error for each time step for all trials
  for(i in 1:nb_time_steps) {
    for(k in 1:nb_trials) {
      pred_error_aux[i, k] = (data.frame(preds_n005[[k]][[j]])$real[i] - data.frame(preds_n005[[k]][[j]])$pred[i])^2
      obs_aux[i, k] = data.frame(preds_n005[[k]][[j]])$real[i]
    }
  }
  # Calculate max - min for each time step and use it normalization factor
  obs_minmax = apply(obs_aux, 1, max) - apply(obs_aux, 1, min)
  
  # Calculate normalized mean square error and its corresponding standard deviation and save 
  NRMSE = sqrt(apply(pred_error_aux, 1, mean))/obs_minmax
  sd_NRMSE = sqrt(apply(pred_error_aux, 1, var))/obs_minmax
  NRMSEs_n005_timestep[, j] = NRMSE
  NRMSEs_n005_timestep_sd[, j] = sd_NRMSE
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
    real = data.frame(preds_n01[[i]][[j]])$real
    pred = data.frame(preds_n01[[i]][[j]])$pred
    NRMSE = nrmse(pred, real, na.rm=TRUE, norm="maxmin")
    NRMSEs_n01[i, j] = NRMSE
    print(NRMSE)
    print("##################")
  }
}

NRMSEs_n01 = data.frame(NRMSEs_n01)
NRMSEs_n01$total = rowSums(NRMSEs_n01)
NRMSEs_n01$index = 1:nb_trials
names(NRMSEs_n01) = c('Cl', 'Cd', 'total', 'index')
NRMSEs_n01_long = gather(NRMSEs_n01, key="variable", value="NRMSEs", 1:2)

# Calculation of max, min, mean, var
mins_n01 = apply(NRMSEs_n01, 2, min)   
means_n01 = apply(NRMSEs_n01, 2, mean) 
vars_n01 = apply(NRMSEs_n01, 2, var)   
maxs_n01 = apply(NRMSEs_n01, 2, max)  

# Calculate average NRMSE and for each time step of each 
NRMSEs_n01_timestep = matrix(0, nb_time_steps, nb_series)
NRMSEs_n01_timestep_sd = matrix(0, nb_time_steps, nb_series)

for(j in 1:nb_series) {
  pred_error_aux = matrix(0, nb_time_steps, nb_trials)
  obs_aux = matrix(0, nb_time_steps, nb_trials)
  
  # Calculate prediction error for each time step for all trials
  for(i in 1:nb_time_steps) {
    for(k in 1:nb_trials) {
      pred_error_aux[i, k] = (data.frame(preds_n01[[k]][[j]])$real[i] - data.frame(preds_n01[[k]][[j]])$pred[i])^2
      obs_aux[i, k] = data.frame(preds_n01[[k]][[j]])$real[i]
    }
  }
  # Calculate max - min for each time step and use it normalization factor
  obs_minmax = apply(obs_aux, 1, max) - apply(obs_aux, 1, min)
  
  # Calculate normalized mean square error and its corresponding standard deviation and save 
  NRMSE = sqrt(apply(pred_error_aux, 1, mean))/obs_minmax
  sd_NRMSE = sqrt(apply(pred_error_aux, 1, var))/obs_minmax
  NRMSEs_n01_timestep[, j] = NRMSE
  NRMSEs_n01_timestep_sd[, j] = sd_NRMSE
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
    print(NRMSE)
    print("##################")
  }
}

NRMSEs_n02 = data.frame(NRMSEs_n02)
NRMSEs_n02$total = rowSums(NRMSEs_n02)
NRMSEs_n02$index = 1:nb_trials
names(NRMSEs_n02) = c('Cl', 'Cd', 'total', 'index')
NRMSEs_n02_long = gather(NRMSEs_n02, key="variable", value="NRMSEs", 1:2)

# Calculation of max, min, mean, var
mins_n02 = apply(NRMSEs_n02, 2, min)  
means_n02 = apply(NRMSEs_n02, 2, mean)
vars_n02 = apply(NRMSEs_n02, 2, var)  
maxs_n02 = apply(NRMSEs_n02, 2, max)  

# Calculate average NRMSE and for each time step of each 
NRMSEs_n02_timestep = matrix(0, nb_time_steps, nb_series)
NRMSEs_n02_timestep_sd = matrix(0, nb_time_steps, nb_series)

for(j in 1:nb_series) {
  pred_error_aux = matrix(0, nb_time_steps, nb_trials)
  obs_aux = matrix(0, nb_time_steps, nb_trials)
  
  # Calculate prediction error for each time step for all trials
  for(i in 1:nb_time_steps) {
    for(k in 1:nb_trials) {
      pred_error_aux[i, k] = (data.frame(preds_n02[[k]][[j]])$real[i] - data.frame(preds_n02[[k]][[j]])$pred[i])^2
      obs_aux[i, k] = data.frame(preds_n02[[k]][[j]])$real[i]
    }
  }
  # Calculate max - min for each time step and use it normalization factor
  obs_minmax = apply(obs_aux, 1, max) - apply(obs_aux, 1, min)
  
  # Calculate normalized mean square error and its corresponding standard deviation and save 
  NRMSE = sqrt(apply(pred_error_aux, 1, mean))/obs_minmax
  sd_NRMSE = sqrt(apply(pred_error_aux, 1, var))/obs_minmax
  NRMSEs_n02_timestep[, j] = NRMSE
  NRMSEs_n02_timestep_sd[, j] = sd_NRMSE
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
  NRMSEs_x = data.frame(index = 1:nb_trials , NRMSEs_nn[, i], 
                        NRMSEs_n005[, i],
                        NRMSEs_n01[, i], 
                        NRMSEs_n02[, i])
  
  names(NRMSEs_x) = c('Index', 'No noise', '0.5% noise', '1% noise', '2% noise')
  NRMSEs_x_noinf = NRMSEs_x[!is.infinite(rowSums(NRMSEs_x)),]
  
  # Concatinate all NRMSE values for all levels of noise into a long df
  NRMSEs_x_long = gather(NRMSEs_x_noinf, key="Noise", value="NRMSE", 2:5)
  
  # Add vector to order noise properly
  order_vec = c(rep(4, 100), rep(3, 100), rep(2, 100), rep(1, 100))
  NRMSEs_x_long = data.frame(NRMSEs_x_long, order_vec)
  
  # Plot estimaton of the PDFs using kernel density
  print(ggplot(NRMSEs_x_long, aes(x = NRMSE, y = fct_reorder(Noise, order_vec, .fun = min), fill = Noise)) +
          geom_density_ridges() +
          xlab("NRMSE") + ylab("Density") +
          theme_ridges() + 
          theme(plot.title = element_text(size = 45,face="bold"), 
                axis.text=element_text(size=40),
                axis.title=element_text(size=45,face="bold"), 
                legend.text = element_text(size=35), 
                legend.title = element_text(size=40), 
                legend.position = "none"))
  
  # Create a dataframe with all NRMSE values per time step of coordinate 
  NRMSEs_x_timestep = data.frame(index = 1:nb_time_steps, NRMSEs_nn_timestep[, i], 
                                 NRMSEs_n005_timestep[, i], 
                                 NRMSEs_n01_timestep[, i], 
                                 NRMSEs_n02_timestep[, i])
  
  names(NRMSEs_x_timestep) = c('Timestep', '0%', '0.5%', '1%', '2%')
  
  # Concatinate all NRMSE values for all levels of noise into a long df
  NRMSEs_x_timestep_long = gather(NRMSEs_x_timestep, key="Noise", value="NRMSE", 2:5)
  
  # Create a dataframe with all standard deviations of the NRMSE values per time step of coordinate 
  NRMSEs_x_timestep_sd = data.frame(index = 1:nb_time_steps, NRMSEs_nn_timestep_sd[, i], 
                                    NRMSEs_n005_timestep_sd[, i], 
                                    NRMSEs_n01_timestep_sd[, i], 
                                    NRMSEs_n02_timestep_sd[, i])
  
  names(NRMSEs_x_timestep_sd) = c('Timestep', '0%', '0.5%', '1%', '2%')
  NRMSEs_x_timestep_long_sd = gather(NRMSEs_x_timestep_sd, key="Noise", value="SD", 2:5)
  
  df_joined = data.frame(NRMSEs_x_timestep_long, "SD" = NRMSEs_x_timestep_long_sd[, 3])
  df_joined$Noise  <- with(df_joined, reorder(Noise, NRMSE))
  
  # Plot the evolution of NRNRMSE over time
  print(ggplot(df_joined, aes(x = Timestep, y = NRMSE, group = Noise)) +
          theme_minimal() +
          theme(plot.title = element_text(size = 50,face="bold"), 
                axis.text=element_text(size=40),
                axis.title=element_text(size=45,face="bold"), 
                legend.text = element_text(size=40),
                legend.title = element_text(size = 45,face="bold")) +
          geom_line(aes(color=Noise), size = 1.7) +
          geom_point(aes(color=Noise), size = 1.7) +
          geom_ribbon(aes(y = NRMSE, ymin = NRMSE - SD, ymax = NRMSE + SD, fill = Noise), alpha = .2))
}


######################################
### Tests to compare distributions ###
######################################
# Kolmogorov-Smirnov test
ks.test(NRMSEs_nn$Cl, NRMSEs_n005$Cl)  
ks.test(NRMSEs_nn$Cl, NRMSEs_n01$Cl)  
ks.test(NRMSEs_nn$Cl, NRMSEs_n02$Cl) 

ks.test(NRMSEs_n005$Cl, NRMSEs_n01$Cl)
ks.test(NRMSEs_n005$Cl, NRMSEs_n02$Cl) 

ks.test(NRMSEs_n01$Cl, NRMSEs_n02$Cl)


ks.test(NRMSEs_nn$Cd, NRMSEs_n005$Cd)  
ks.test(NRMSEs_nn$Cd, NRMSEs_n01$Cd) 
ks.test(NRMSEs_nn$Cd, NRMSEs_n02$Cd) 

ks.test(NRMSEs_n005$Cd, NRMSEs_n01$Cd) 
ks.test(NRMSEs_n005$Cd, NRMSEs_n02$Cd) 

ks.test(NRMSEs_n01$Cd, NRMSEs_n02$Cd)