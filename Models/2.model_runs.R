### Linear Gaussian SSM -- comparison of dac-SMC, stpf, nsmc, bootstrap

setwd("./smchd")

options(scipen = 999)

library(data.table)
library(tictoc)
library(MASS)
library(LaplacesDemon)

source('./R/utils.R')
source('./R/marginal_dac_lgssm.R')
source('./R/marginal_lgssm_light_fixed_theta.R')
source('./R/nsmc_lgssm.R')
source('./R/stpf_lgssm.R')
source('./R/bootstrap_lgssm.R')


library(doSNOW)
library(foreach)

# progress bar ------------------------------------------------------------
library(progress)
no_cores <- parallel::detectCores() - 1
cl <- makeCluster(20)
registerDoSNOW(cl)

iterations <- 20                               # used for the foreach loop  

pb <- progress_bar$new(
  format = "letter = :letter [:bar] :elapsed | eta: :eta",
  total = iterations,    # 100 
  width = 60)

progress_letter <- rep(LETTERS[1:10], 10)  # token reported in progress bar

# allowing progress bar to be used in foreach
progress <- function(n){pb$tick(tokens = list(letter = progress_letter[n]))} 
opts <- list(progress = progress)

# parameters
tau <- 1
lambda <- 0
sigmaY <- 0.5^2 # 2^0.5
Time.step <- 50
iterations <- 20

foreach (ID = 1:iterations, .options.snow = opts) %dopar% {
  set.seed(1234*ID)
  pb$tick()
  
  for (d in c(16, 32, 64, 256)){
    # initial state
    mu0 <- rep(0, times = d)
    Sigma0 <- diag(x = 1, d, d)
    
    for (Nparticles in c(100, 500, 1000)){
      M <- 100
      # observations
      filename <- paste0('./data256/data_lgssm_d', d, "ID", ID)
      
      df_nsmc <- data.frame()
      df_stpf <- data.frame()
      df_dac <- data.frame()
      df_boot <- data.frame()
      
      # initial distribution
      res_nsmc <- MASS::mvrnorm(n = Nparticles, mu0, Sigma0)
      res_stpf <- array(MASS::mvrnorm(n = Nparticles*M, mu0, Sigma0), dim = c(Nparticles, M, d))
      res_dac <- res_nsmc
      res_boot <- res_nsmc
      
      
      for (t in 1:Time.step){
        print(paste0("Bootsrap:",ID, "     Dim:",d, "     N_particles:", Nparticles, "     Time-step:", t))
        y <- unname(data.matrix(read.csv(filename, row.names = 1, nrows = 1, skip = t-1)))[1:d]
        
        # bootstrap
        start_boot <- Sys.time()
        res_boot <- bootstrap_lgssm(res_boot, y, tau, lambda, sigmaY)
        end_boot <- Sys.time()
        rt <- as.numeric(difftime(end_boot, start_boot, units = "secs"))
        #rt <- as.numeric(gsub("[^0-9.<>]", "", toclist$callback_msg))
        print(paste0("seconds passed: ",rt))
        df_boot <- data.frame(rbind(df_boot, cbind(colMeans(res_boot), t, rt)))
        
        # dac (lightweight)
        start_dac <- Sys.time()
        res_dac <- marginal_dac_lgssm_lightweight(res_dac, y, tau, lambda, sigmaY)
        end_dac <- Sys.time()
        rt <- as.numeric(difftime(end_dac, start_dac, units = "secs"))
        #rt <- as.numeric(gsub("[^0-9.<>]", "", toclist$callback_msg))
        print(paste0("seconds passed: ",rt))
        df_dac <- data.frame(rbind(df_dac, cbind(colMeans(res_dac), t, rt)))
        
        # nsmc
        start_nsmc <- Sys.time()
        res_nsmc <- nsmc_lgssm(res_nsmc, y, tau, lambda, sigmaY, M)
        end_nsmc <- Sys.time()
        rt <- as.numeric(difftime(end_nsmc, start_nsmc, units = "secs"))
        print(paste0("seconds passed: ",rt))
        df_nsmc <- data.frame(rbind(df_nsmc, cbind(colMeans(res_nsmc), t, rt)))
        
        # stpf
        start_stpf <- Sys.time()
        res_stpf <- stpf_lgssm(res_stpf, y, tau, lambda, sigmaY)
        end_stpf <- Sys.time()
        rt <- as.numeric(difftime(end_stpf, start_stpf, units = "secs"))
        print(paste0("seconds passed: ",rt))
        df_stpf <- data.frame(rbind(df_stpf, cbind(colMeans(res_stpf, dims = 2), t, rt)))
      }
      
      # save particles at last time step for W1 and KS
      filename <- paste0("./data256/results/t50_dac_lgssm_d", d, "N", Nparticles, "ID", ID)
      write.table(res_dac, file = filename, append = FALSE, sep = " ", dec = ".",
                  row.names = TRUE, col.names = TRUE)
      filename <- paste0("./data256/results/t50_nsmc_lgssm_d", d, "N", Nparticles, "ID", ID)
      write.table(res_nsmc, file = filename, append = FALSE, sep = " ", dec = ".",
                  row.names = TRUE, col.names = TRUE)
      filename <- paste0("./data256/results/t50_stpf_lgssm_d", d, "N", Nparticles, "ID", ID)
      write.table(matrix(res_stpf, ncol = d, nrow = Nparticles*M), file = filename, append = FALSE, sep = " ", dec = ".",
                  row.names = TRUE, col.names = TRUE)
      filename <- paste0("./data256/results/t50_boot_lgssm_d", d, "N", Nparticles, "ID", ID)
      write.table(res_boot, file = filename, append = FALSE, sep = " ", dec = ".",
                  row.names = TRUE, col.names = TRUE)
      
      # save means
      write.csv(x=df_dac, file=paste0("./data256/results/dac_lgssm_d", d, "N", Nparticles, "ID", ID))
      write.csv(x=df_nsmc, file=paste0("./data256/results/nsmc_lgssm_d", d, "N", Nparticles, "ID", ID))
      write.csv(x=df_stpf, file=paste0("./data256/results/stpf_lgssm_d", d, "N", Nparticles, "ID", ID))
      write.csv(x=df_boot, file=paste0("./data256/results/boot_lgssm_d", d, "N", Nparticles, "ID", ID))
    }
  }
}
