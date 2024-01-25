# RMSE for linear Gaussian SSM
library(ggpubr)

file_list =list.files("./data/lgssm_results/", pattern="lgssm")
DF <-  read.csv(paste0("./data/lgssm_results/", file_list[1]))
idx_in_DF <- unlist(regmatches(file_list[1], gregexpr('\\(?[0-9]+', file_list[1])))
DF$d <- idx_in_DF[1]
DF$N <- idx_in_DF[2]
DF$run <- as.numeric(idx_in_DF[3])

#reading each file within the range and append them to create one file
for (f in file_list[-1]){
  current_file <- read.csv(paste0("./data/lgssm_results/",f))
  idx_in_file <- unlist(regmatches(f, gregexpr('\\(?[0-9]+', f)))
  current_file$d <- idx_in_file[1]
  current_file$N <- idx_in_file[2]
  current_file$run <- as.numeric(idx_in_file[3])
  DF <- rbind(DF, current_file)    # append the current file
}

df <- DF[, -1]
Time.step <- ncol(df) - 7
colnames(df)[(Time.step+1):(Time.step+3)] <- c("w1", "ks", "runtime")
