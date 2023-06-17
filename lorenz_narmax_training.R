###################
### Train model ###
###################
# Optimal parameters
MSEs_df = readRDS("Results/narmax_MSEs.RData")
opt_n = MSEs_df[which(MSEs_df$MSEs == min(MSEs_df$MSEs)), ]

# Define model
narmax_model = narmax(opt_n$ny, opt_n$nu, opt_n$ne, opt_n$nl)

# Train model
model_narmax = estimate.narmax(model_narmax, x_train, u_train, opt_n$rho_p, opt_n$rho_e)

model_narmax$nb_terms
model_narmax$coefficients
model_narmax$terms

# Save model 
saveRDS(model_narmax, file="Results/narmax_model.RData")
