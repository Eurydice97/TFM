###############################
### Feature matrix creation ###
###############################
xu_terms_train = xu_train
col_names = colnames(xu_train)

# Add sinus of each term as feature
for(j in 1:ncol(xu_train)) {
  xu_terms_train = cbind(xu_terms_train, sin(2*pi*xu_train[, j]))
  col_name = paste0('sin(', colnames(xu_train)[j], ')')
  col_names = c(col_names, col_name)
}

# Add cosinus of each term as feature
for(j in 1:ncol(xu_train)) {
  xu_terms_train = cbind(xu_terms_train, cos(2*pi*xu_train[, j]))
  col_name = paste0('cos(', colnames(xu_train)[j], ')')
  col_names = c(col_names, col_name)
}

# Add exponential of each term as feature
for(j in 1:ncol(xu_train)) {
  xu_terms_train = cbind(xu_terms_train, exp(-0.5*xu_train[, j]))
  col_name = paste0('neg_exp(', colnames(xu_train)[j], ')')
  col_names = c(col_names, col_name)
}

for(j in 1:ncol(xu_train)) {
  xu_terms_train = cbind(xu_terms_train, exp(0.5*xu_train[, j]))
  col_name = paste0('pos_exp(', colnames(xu_train)[j], ')')
  col_names = c(col_names, col_name)
}

colnames(xu_terms_train) = col_names

# Creates data matrix with terms up to degree 3
feat_mat_train = sindyr::features(xu_terms_train, polyorder = 3)


###################
### Train model ###
###################
# Optimal lambda
MSEs_df = readRDS("Results/sindy_MSEs.RData")
MSEs_df_no_na = na.omit(MSEs_df)
opt_lambda = MSEs_df_no_na[MSEs_df_no_na$MSEs == min(MSEs_df_no_na$MSEs), ]$lambdas[1]

# Train model
sindy = sindy(as.matrix(xu_train), Theta = feat_mat_train, lambda = opt_lambda, dt = 0.1)

# Create dataframe containing predictions for all time series
coeff = data.frame(sindy$B)
coeff

# Save model
saveRDS(sindy_model, file="Results/sindy_model.RData")
