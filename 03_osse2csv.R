source("C:/Users/cherr/Dropbox/VS Code/research/RA_GH/01_func.R")
source("C:/Users/cherr/Dropbox/VS Code/research/RA_GH/02_gamlss.R")


n <- 10000
rand <- matrix(NA, nrow = n, ncol = 120)
for (i in 1:n) {
  generated <- osse(mu, sigma)
  rand[i, ] <- generated
}

# export to csv file
rand_df <- as.data.frame(rand)
write.csv(rand_df, file = "osse_df.csv", row.names = FALSE) # specify path to store the file
# export to binary file
# writeBin(as.vector(unlist(rand_df)), "osse_df.bin")


# comparison between truth and generated data
# osse_dat <- osse(mu, sigma)
# plot(amax$year, amax$outflow, col = "blue", bg = "lightblue", pch = 21, xlab = "year", ylab = "annual max outflow [m^3/s]", main = "OSSE generation")
# points(amax$year, osse_dat, col = "red", bg = "pink", pch = 21)
# legend("topleft", legend = c("truth", "generated (osse)"), col = c("blue", "red"), pch = 21, pt.bg = c("lightblue", "pink"), cex = 0.8)


# export random generation of custom truth values to csv file