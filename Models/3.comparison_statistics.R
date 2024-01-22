### Linear Gaussian SSM -- comparison of dac, stpf, nsmc, bootstrap

setwd("./smchd")
library(transport)
source('./R/utils.R')
source('./R/marginal_lgssm_light_fixed_theta.R')
source('./R/nsmc_lgssm.R')
source('./R/stpf_lgssm.R')
source('./R/bootstrap_lgssm.R')


library(LaplacesDemon)

library(doSNOW)
no_cores <- parallel::detectCores() - 1
cl <- makeCluster(20)
registerDoSNOW(cl)
pb <- txtProgressBar(max = 20, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# parameters
tau <- 1
lambda <- 1
sigmaY <- 0.5^2 # 2^0.5
Time.step <- 50
iterations <- 20
M <- 100 # for stpf

foreach (ID = 1:iterations) %dopar%{
  set.seed(1234*ID)
  
  for (d in c(16, 32, 64, 256)){
    # initial state
    mu0 <- rep(0, times = d)
    Sigma0 <- diag(x = 1, d, d)
    
    for (Nparticles in c(100, 500, 1000)){
      # get data
      filename <- paste0("./data256/data_lgssm_d", d, "ID", ID)
      true_means <- unname(data.matrix(read.csv(filename, row.names = 1, nrows=Time.step, skip=Time.step)))[, 1:d]
      true_variances <- unname(data.matrix(read.csv(filename, row.names = 1, nrows=Time.step, skip=2*Time.step)))[, 1:d]
      
      last_t_nsmc <- unname(data.matrix(read.table(paste0("./data256/results/t50_nsmc_lgssm_d", d, "N", Nparticles, "ID", ID), row.names = 1)))
      last_t_dac <- unname(data.matrix(read.table(paste0("./data256/results/t50_dac_lgssm_d", d, "N", Nparticles, "ID", ID), row.names = 1)))
      last_t_stpf <- unname(data.matrix(read.table(paste0("./data256/results/t50_stpf_lgssm_d", d, "N", Nparticles, "ID", ID), row.names = 1)))
      last_t_stpf_kl <- aggregate(last_t_stpf, list(rep(1:(nrow(last_t_stpf) %/% M + 1),
                                                     each = M, len = nrow(last_t_stpf))), mean)[-1]
      last_t_boot <- unname(data.matrix(read.table(paste0("./data256/results/t50_boot_lgssm_d", d, "N", Nparticles, "ID", ID), row.names = 1)))
      
      marginals <- matrix(0, nrow = Nparticles*Time.step, ncol = d)
      
      # KL Divergence
      res_dac_kl = vector("numeric", length = d)
      res_nsmc_kl = vector("numeric", length = d)
      res_stpf_kl = vector("numeric", length = d)
      res_boot_kl = vector("numeric", length = d)
      
      for(i in 1:d){
        marginals[, i] <- rnorm(Nparticles*Time.step, mean = true_means[Time.step, i],
                                sd = sqrt(true_variances[Time.step, i]))
        tmp <- tail(marginals[, i], Nparticles*1)
        res_dac_kl[i]  <- LaplacesDemon::KLD(tmp, last_t_dac[, i])$mean.sum.KLD
        res_nsmc_kl[i] <- LaplacesDemon::KLD(tmp, last_t_nsmc[, i])$mean.sum.KLD
        res_stpf_kl[i] <- LaplacesDemon::KLD(tmp, last_t_stpf_kl[, i])$mean.sum.KLD
        res_boot_kl[i] <- LaplacesDemon::KLD(tmp, last_t_boot[, i])$mean.sum.KLD
      }
      
      df <- data.frame()
      data_dac <- read.csv(paste0("./data256/results/dac_lgssm_d", d, "N", Nparticles, "ID", ID), row.names = 1)
      data_nsmc <- read.csv(paste0("./data256/results/nsmc_lgssm_d", d, "N", Nparticles, "ID", ID), row.names = 1)
      data_stpf <- read.csv(paste0("./data256/results/stpf_lgssm_d", d, "N", Nparticles, "ID", ID), row.names = 1)
      data_boot <- read.csv(paste0("./data256/results/boot_lgssm_d", d, "N", Nparticles, "ID", ID), row.names = 1)
      
      rse_dac <- (unname(matrix(data_dac$V1, ncol = d, byrow = TRUE)) - true_means)^2/true_variances
      rse_nsmc <- (unname(matrix(data_nsmc$V1, ncol = d, byrow = TRUE))  - true_means)^2/true_variances
      rse_stpf <- (unname(matrix(data_stpf$V1, ncol = d, byrow = TRUE))  - true_means)^2/true_variances
      rse_boot <- (unname(matrix(data_boot$V1, ncol = d, byrow = TRUE))  - true_means)^2/true_variances
      
      res_dac_ks <- apply(rbind(last_t_dac, marginals), MARGIN = 2, ks_dist, N = Nparticles)
      res_dac_w1 <- apply(rbind(last_t_dac, marginals), MARGIN = 2, w1_dist, N = Nparticles)

      res_nsmc_ks <- apply(rbind(last_t_nsmc, marginals), MARGIN = 2, ks_dist, N = Nparticles)
      res_nsmc_w1 <- apply(rbind(last_t_nsmc, marginals), MARGIN = 2, w1_dist, N = Nparticles)
      
      res_stpf_ks <- apply(rbind(last_t_stpf, marginals), MARGIN = 2, ks_dist, N = Nparticles)
      res_stpf_w1 <- apply(rbind(last_t_stpf, marginals), MARGIN = 2, w1_dist, N = Nparticles)
      
      res_boot_ks <- apply(rbind(last_t_boot, marginals), MARGIN = 2, ks_dist, N = Nparticles)
      res_boot_w1 <- apply(rbind(last_t_boot, marginals), MARGIN = 2, w1_dist, N = Nparticles)
      
      df1 <- data.frame(rbind(df, cbind(t(rse_dac), res_dac_w1, res_dac_ks, res_dac_kl, rep(sum(data_dac$rt), times = d))))
      colnames(df1)[(Time.step+1):(Time.step+4)] <- c("remse", "w1", "ks", "kl", "rt")
      df2 <- data.frame(rbind(df, cbind(t(rse_nsmc), res_nsmc_w1, res_nsmc_ks, res_nsmc_kl, rep(sum(data_nsmc$rt), times = d))))
      colnames(df2)[(Time.step+1):(Time.step+4)] <- c("remse", "w1", "ks", "kl", "rt")
      df3 <- data.frame(rbind(df, cbind(t(rse_stpf), res_stpf_w1, res_stpf_ks, res_stpf_kl, rep(sum(data_stpf$rt), times = d))))
      colnames(df3)[(Time.step+1):(Time.step+4)] <- c("remse", "w1", "ks", "kl", "rt")
      df4 <- data.frame(rbind(df, cbind(t(rse_boot), res_boot_w1, res_boot_ks, res_boot_kl, rep(sum(data_boot$rt), times = d))))
      colnames(df4)[(Time.step+1):(Time.step+4)] <- c("remse", "w1", "ks", "kl", "rt")
      
      df <- rbind(df1, df2, df3, df4)
      df$algo <- as.factor(rep(c("dac-light", "nsmc", "stpf", "boot"), each = d))
      
      
      filename <- paste0("./data256/lgssm_results/lgssm_d", d, "N", Nparticles, "ID", ID)
      write.csv(x=df, file=filename)
    }
  }
}