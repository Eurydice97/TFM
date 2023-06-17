###########################
### Necessary functions ###
###########################
# Sindy prediction
# Lorenz simulation
# Function that simulates a Lorenz model
# y: initial condition
# u: exogenous parameter
sindy_pred = function(x, u, coeff) {
  
  xu = data.frame(t(x), u)

  #xu_terms = xu
  colnames(xu) = c("Cl", "Cd", "u")
  #col_names = c("Cl", "Cd", "u")
  #colnames(xu_terms) = col_names
  
  # Add sinus of each term as feature
  #for(j in 1:ncol(xu)) {
  #  xu_terms = cbind(xu_terms, sin(xu[, j]))
  #  col_name = paste0('sin(', colnames(xu)[j], ')')
  #  col_names = c(col_names, col_name)
  #}

  # Add cosinus of each term as feature
  #for(j in 1:ncol(xu)) {
  #  xu_terms = cbind(xu_terms, cos(xu[, j]))
  #  col_name = paste0('cos(', colnames(xu)[j], ')')
  #  col_names = c(col_names, col_name)
  #}
  
  # Add exponential of each term as feature
  #for(j in 1:ncol(xu)) {
  #  xu_terms = cbind(xu_terms, exp(-0.5*xu[, j]))
  #  col_name = paste0('neg_exp(', colnames(xu)[j], ')')
  #  col_names = c(col_names, col_name)
  #}
  
  #for(j in 1:ncol(xu)) {
  #  xu_terms = cbind(xu_terms, exp(0.5*xu[, j]))
  #  col_name = paste0('pos_exp(', colnames(xu)[j], ')')
  #  col_names = c(col_names, col_name)
  #}
  #print(col_names)
  #colnames(xu_terms) = col_names

  # Creates data matrix with terms up to degree 3
  features = sindyr::features(xu, polyorder = 4)
  #features = sindyr::features(xu_terms, polyorder = 4)

  pred = c(features %*% coeff[, 1],
           features %*% coeff[, 2])
  
  return(pred)
}

# Integration function
# Function that uses Runge-Kutta to integrate the result of a given function
# for a given amount ot time steps ahead
# v: function the result of which needs to be integrated
# y: initial condition
# u: exogenous parameter of length n
# h: size of time step
# p: further parameters of function v
rk4u = function(v, y, u, h, p) { 
  
  # RK4U   Runge-Kutta scheme of order 4 for control system
  k1 = v(y, u, p)
  k2 = v(y + (h/2)*k1, u, p)
  k3 = v(y + (h/2)*k2, u, p)
  k4 = v(y + h*k3, u, p)
  y = y + h*(k1 + 2*k2 + 2*k3 + k4)/6
  
  return(y)
}
