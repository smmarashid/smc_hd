par(mfrow = c(1,3))
rmse_dataB = rmse_data[rmse_data$N == "2000" | rmse_data$d == "8",]

boxplot(rmse_dataB$rmse ~ rmse_dataB$algo, 
        xlab= "Algorithm",
        ylab = "ReMSE")
title("ReMSE for d=8, N=2000")

rmse_dataB = rmse_data[rmse_data$N == "2000" | rmse_data$d == "16",]
boxplot(rmse_dataB$rmse ~ rmse_dataB$algo, 
        xlab= "Algorithm",
        ylab = "ReMSE")
title("ReMSE for d=16, N=2000")

rmse_dataB = rmse_data[rmse_data$N == "2000" | rmse_data$d == "32",]
boxplot(rmse_dataB$rmse ~ rmse_dataB$algo, 
        xlab= "Algorithm",
        ylab = "ReMSE")
title("ReMSE for d=32, N=2000")