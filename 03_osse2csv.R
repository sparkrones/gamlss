source("C:/Users/cherr/Dropbox/VS Code/research/RA_GH/02_gamlss.R")


n <- 100
osse_list <- list()

for (i in 1:n) {
  generated <- osse(mu, sigma, amax)
  osse_list[[paste0("outflow_", i)]] <- generated$outflow
  osse_list[[paste0("q2_", i)]] <- generated$q2
  osse_list[[paste0("q3_", i)]] <- generated$q3
  max_outflow <- max(unlist(generated$outflow))
}


# export generated amax to csv file
rand <- do.call(cbind, osse_list[grep("^outflow_", names(osse_list))])
write.csv(rand, file = "shimanto_osse_df.csv", row.names = FALSE) # specify path to store the file
# export to binary file
# writeBin(as.vector(unlist(rand)), "osse_df.bin")


# check generated data variations
# calculate the truth 3-year & 2-year floods
for (i in seq_along(df$year)) {
  t_q2[i] <- qGU((1 - 1/2), mu = mu[i], sigma = sigma[i])
  t_q3[i] <- qGU((1 - 1/3), mu = mu[i], sigma = sigma[i])
}

# plot every 50 osse
num_plot <- sample(1:n, 100, replace = FALSE)

for (i in seq(1, length(num_plot), by = 50)) {
  pdf(paste0("plot_", i, ".pdf"), width = 20, height = 20)
  # par(mfrow = c(4, 2), mar = c(2, 2, 2, 2))
  par(mfrow = c(10, 5), mar = c(2, 2, 2, 2))
  
  for (j in 0:49) {
    index <- i + j
    if (index <= length(num_plot)) {
      outflow <- osse_list[[paste0("outflow_", num_plot[index])]]
      n_df <- cbind(data.frame(df$year, outflow))
      colnames(n_df) <- c("year", "outflow")
      
      # 100-year estimation
      n_model <- gamlss(outflow ~ year, mu.fo = ~ year, sigma.fo = ~ year, family = "GU", data = n_df)
      n_mu <- lpred(n_model, what = "mu", type = "response")
      n_sigma <- lpred(n_model, what = "sigma", type = "response")
      
      n_hundred <- qGU(0.99, mu = n_mu, sigma = n_sigma)
      
      
      # plot
      # amax
      plot(df$year, df$outflow, type = "p", col = "#595959", bg = "gray", pch = 21, cex = 0.5, ylim = c(0, max_outflow+100), main = paste0("osse_", num_plot[index]), xlab = "year", ylab = "amax")
      points(df$year, outflow, col = "blue", bg = "lightblue", pch = 21, cex = 0.5)
      
      # 100-year
      lines(df$year, df$hundred_f, col = "red", lwd = 2)
      lines(df$year, n_hundred, col = "#3679ff", lwd = 2)
      
      # truth q2 & q3
      points(df$year, t_q2, col = "gray", pch = 21, cex = 0.5)
      points(df$year, t_q3, col = "gray", pch = 21, cex = 0.5)
      # generated q2 & q3
      points(df$year, osse_list[[paste0("q2_", num_plot[index])]], col = "#3679ff", pch = 21, cex = 0.5)
      points(df$year, osse_list[[paste0("q3_", num_plot[index])]], col = "#3679ff", pch = 21, cex = 0.5)
      
      legend("topleft", 
             legend = c("original amax", "generated amax", "100-year truth", "100-year estimated", "truth 2-&3-year", "generated 2-&3-year"), 
             cex = 0.5, 
             col = c("#595959", "blue", "red", "#3679ff", "gray", "#3679ff"), 
             pch = c(21, 21, NA, NA, 21, 21), 
             pt.bg = c("gray", "lightblue", NA, NA, NA, NA), 
             lwd = c(NA, NA, 1, 1, NA, NA))
    }
  }
  
  dev.off()
}
