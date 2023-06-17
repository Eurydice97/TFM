###################
### Train model ###
###################
# Choose combination that has the lowest MSE
MSEs_df = readRDS("Results/narmax_MSEs.RData")
opt_ns = MSEs_df[which(MSEs_df$MSEs == min(MSEs_df$MSEs)), ]
opt_n = opt_ns[nrow(opt_ns),]

# Define model
model_narmax = narmax(opt_n$ny, opt_n$nu, opt_n$ne, opt_n$nl)

# Train model
model_narmax = estimate.narmax(model_narmax, x_train, u_train, opt_n$rho_p, opt_n$rho_e)

model_narmax$terms
model_narmax$coefficients
model_narmax$nb_terms

# Save model
saveRDS(model_narmax, file="Results/narmax_model.RData")
