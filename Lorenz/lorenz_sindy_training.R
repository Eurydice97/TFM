###################
### Train Model ###
###################
# Creates data matrix with terms up to degree 2
feat_mat_train = sindyr::features(as.matrix(xu_train), polyorder = 2)

# Optimal parameters
MSEs_df = readRDS("Results/sindy_MSEs.RData")
opt_lambda = MSEs_df[MSEs_df$MSEs == min(MSEs_df$MSEs), ]$lambdas[1]

# Train model
sindy_model = sindy(as.matrix(xu_train), dt = dt, Theta = feat_mat_train, lambda = opt_lambda)

# Create dataframe containing predictions for all time series
coeff = data.frame(sindy$B)
coeff

# Save model
saveRDS(sindy_model, file="Results/sindy_model.RData")
