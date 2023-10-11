
#' Generate next value of g
#'
#' @param alpha Parameter of model
#' @param beta Parameter of model
#' @param gamma Parameter of model
#' @param g Previous step of g
#' @param Z Randomly sampled residual
#'
#' @return g at t+1
next_g_func <- function(alpha, beta, gamma, Z, g) {
  return((1 - alpha - gamma / 2 - beta) + (alpha + gamma * as.numeric(Z < 0)) *
           (Z ** 2) *g + beta * g)
}
x=get(GM_models_list[[6]])
g = x$g[[length(x$g)]] # last value of g = fitted short-term component
alpha = x$par["alpha"][[1]]
beta = x$par["beta"][[1]]
gamma = x$par["gamma"][[1]]
K = x$K
df_residuals<-x$df.fitted$residuals

set.seed(42)
Z = sample(df_residuals[K+1:length(df_residuals)], size=1, replace=TRUE) # one sampled residual
g_1<-next_g_func(alpha, beta, gamma, Z, g)



#' Get Bootstrap Prediction Intervals
#'
#' @param alpha Parameter of model
#' @param beta Parameter of model
#' @param gamma Parameter of model
#' @param g0 Last observation available
#' @param h Timesteps to predict into the future
#' @param q_upper Upper quantile to predict
#' @param q_lower Lower quantile to predict
#' @param B Number of bootstrap samples
#'
#' @return Vector of quantiles for prediction range n_forecasts
get_bootstrap_pi <- function(alpha, beta, gamma, g0, df_resids, n_forecasts, q_upper = 0.95,
                             q_lower = 0.05, B = 1000){
  # Generate random draws
  Z_bootstrap <- sample(df_resids, size=B, replace=TRUE)
  #Create results matrix
  bootstrap_matrix <- array(data = 0, dim = c(n_forecasts, B))
  #First step
  bootstrap_matrix[1,] <- next_g_func(alpha, beta, gamma, Z=Z_bootstrap, g0)
  #Consecutive steps
  delta = alpha + gamma / 2 + beta
  if(n_forecasts > 1){
    for (i in 2:n_forecasts) { # recursion
      bootstrap_matrix[i,]<-(1-delta) + delta* bootstrap_matrix[(i-1),]
    }
  }
  
  #Get quantiles
  quantiles <- apply(bootstrap_matrix, MARGIN = 1, quantile, c(q_lower, q_upper))
  return(quantiles)
}




# Test
x=GM_vix
df_residuals<-x$df.fitted$residuals
K = x$K
g = x$g[[length(x$g)]] # last value of g = fitted short-term component
alpha = x$par["alpha"][[1]]
beta = x$par["beta"][[1]]
gamma = x$par["gamma"][[1]]
set.seed(42)
quantiles <- get_bootstrap_pi(alpha, beta, gamma, g0=g, df_resids=df_residuals[(K+1):length(df_residuals)], n_forecasts=3)
print(quantiles)





