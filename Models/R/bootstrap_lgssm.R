# Bootsrap particle filter for linear Gaussian SS
bootstrap_lgssm <- function(xOld, obs, tau, lambda, sigmaY){
  # dimension, number islands and number of particles
  d <- dim(xOld)[2]
  Nparticles <- nrow(xOld)
  
  x <- matrix(0, nrow = Nparticles, ncol = d)
  lW <- matrix(0, nrow = Nparticles, ncol = d)
  W <- matrix(0, nrow = Nparticles, ncol = d)
  
  # loop over dimensions
  # j=1
  j <- 1
  x[, j] <- 1.00*xOld[j] + rnorm(Nparticles)/sqrt(tau)
  
  # weights
  lW <- -1.00*(obs[j] - x[, j])^2/sigmaY - 1.00*log(2*pi*sigmaY)
  max.lW <- max(lW)
  W <- exp(lW - max.lW)
  
  # resampling
  W <- W/sum(W)
  ancestors <- stratified_resample(W, Nparticles)
  xOld[, ] <- xOld[ancestors, ]
  x[, j] <- x[ancestors, j]
  
  for(j in 2:d){
    # propose
    x[, j] <- (1.00*tau*xOld[j] + lambda*x[, j-1])/(tau+lambda) + rnorm(Nparticles)/sqrt(tau+lambda)
    # weights
    lW <- -1.00*(obs[j] - x[, j])^2/sigmaY - 1.00*log(2*pi*sigmaY)
    max.lW <- max(lW)
    W <- exp(lW - max.lW)
    # resampling
    W <- W/sum(W)
    ancestors <- stratified_resample(W, Nparticles)
    xOld[, ] <- xOld[ancestors, ]
    x[, 1:j] <- x[ancestors, 1:j]
  }
  return(x)
}