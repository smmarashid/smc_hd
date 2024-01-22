# Get data for linear Gaussian SSM
setwd("./smchd")

library(MASS)
library(FKF)
source("./R/ssm_obs.R")

# parameters
tau <- 1
lambda <- 0
sigmaY <- 0.5^2 # 2^0.5
Time.step <- 50
iterations <- 20

for (ID in 1:iterations){
  set.seed(1234*ID)
  
  for (d in c(16, 32, 64, 256)){
    # initial state
    mu0 <- rep(0, times = d)
    sigma0 <- diag(x = 1, d, d)
    
    # coefficient and precision of the state equation
    m1 <- diag(x = tau, d, d)
    m1[1, 1] <- tau + lambda
    m2 <- diag(x = tau+lambda, d, d)
    m2[row(m2) - col(m2) == 1] <- -lambda
    m3 <- diag(x = tau+lambda, d, d)
    m3[1, 1] <- tau
    x.coeff <- 1.00*m1%*%solve(m2)
    x.error.prec <- (t(m2)%*%m3%*%m2)/(tau+lambda)^2
    x.error.var <- solve(x.error.prec)
    
    # coefficient and covariance of the observation equation
    y.error.var <- diag(x = sigmaY, d, d)
    y.coeff <- diag(x = 1, d, d)

    # get observations
    y <- ssm_obs(mu0, sigma0, y.coeff, x.coeff, x.error.prec, y.error.var, Time.step)
    
    # Kalman filter
    res_KF <- fkf(a0 = mu0, P0 = 1.0^2*t(x.coeff)%*%sigma0%*%x.coeff + x.error.var, dt = as.matrix(rep(0, times = d)),
                  ct = as.matrix(rep(0, times = d)), Tt = x.coeff, Zt = y.coeff, HHt = x.error.var, GGt = y.error.var, yt = t(y))
    true_ll <- res_KF$logLik
    true_means <- t(res_KF$att)
    true_variances <- matrix(0, ncol = d, nrow = Time.step)
    for (t in 1:Time.step){
      true_variances[t, ] <- diag(res_KF$Ptt[, , t])
      }
    
    filename <- paste0("./data256/data_lgssm_d", d, "ID", ID)
    df_kf <- data.frame(rbind(y, true_means, true_variances))
    df_kf$type <- as.factor(rep(c("obs", "mean", "var"), each = Time.step))
    write.csv(x=df_kf, file=filename)
  }
}