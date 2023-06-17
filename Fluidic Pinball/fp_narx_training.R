###################
### Train model ###
###################
# Optimal parameters
MSEs_df = readRDS("Results/narx_MSEs.RData")
MSEs_df = MSEs_df[MSEs_df$MSEs != 0, ]
MSEs_df = na.omit(MSEs_df) 
opt_ns = MSEs_df[which(MSEs_df$MSEs == min(MSEs_df$MSEs)), ]
opt_n = opt_ns[nrow(opt_ns),]

# Define model
model_narx = narx(opt_n$ny, opt_n$nu, opt_n$nl)

# Train model
#model_narx = estimate.narx(model_narx, x_train, u_train, c(opt_n$rho1, opt_n$rho2))
model_narx = estimate.narx(model_narx, x_train, u_train, opt_n$rho)

model_narx$nb_terms
model_narx$coefficients
model_narx$terms

# Save model
saveRDS(model_narx, file="Results/narx_model.RData")
