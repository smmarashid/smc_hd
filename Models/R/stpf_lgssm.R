# Space-time particle filter for linear Gaussian SSM
stpf_lgssm <- function(xOld, obs, tau, lambda, sigmaY){
  # dimension, number islands and number of particles
  d <- dim(xOld)[3]
  Nparticles <- nrow(xOld)
  M <- ncol(xOld)

  x <- array(0, dim = c(Nparticles, M, d))
  lZ <- rep(0, times = Nparticles)
  # loop over islands
  for(i in 1:Nparticles){
    # loop over dimension
    # j=1
    j <- 1
    x[i, , j] <- 1.00*xOld[i, , j] + rnorm(M)/sqrt(tau) ###0.5
    # weights
    lW <- -1.00*(obs[j] - x[i, , j])^2/sigmaY - 1.00*log(2*pi*sigmaY) ###0.5
    max.lW <- max(lW)
    W <- exp(lW - max(lW))
    lZ[i] <- log(mean(W)) + max.lW
    # resampling
    W <- W/sum(W)
    ancestors <- stratified_resample(W, M)
    xOld[i, , ] <- xOld[i, ancestors, ]
    x[i, , j] <- x[i, ancestors, j]
    for(j in 2:d){
      # propose
      x[i, , j] <- (1.00*tau*xOld[i, , j] + lambda*x[i, , j-1])/(tau+lambda) + rnorm(M)/sqrt(tau+lambda) #0.5
      # weights
      lW <- -1.00*(obs[j] - x[i, , j])^2/sigmaY - 1.00*log(2*pi*sigmaY) #0.5
      max.lW <- max(lW)
      W <- exp(lW - max(lW))
      lZ[i] <- lZ[i] + log(mean(W)) + max.lW
      # resampling
      W <- W/sum(W)
      ancestors <- stratified_resample(W, M)
      xOld[i, , ] <- xOld[i, ancestors, ]
      x[i, , 1:j] <- x[i, ancestors, 1:j]
    }
  }
  # resampling islands
  Wisland <- exp(lZ - max(lZ))
  Wisland <- Wisland/sum(Wisland)
  ancestors_island <- stratified_resample(Wisland, Nparticles)
  x <- x[ancestors_island, ,]
  return (x)
}
