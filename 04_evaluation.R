source("C:/Users/cherr/Dropbox/VS Code/research/RA_GH/00_func.R")
source("C:/Users/cherr/Dropbox/VS Code/research/RA_GH/02_gamlss.R")

# import osse generation data
osse_data <- read.csv("C:/Users/cherr/Dropbox/VS Code/research/RA_GH/data/shimanto_osse_df.csv")
colnames(osse_data) <- c("year", "outflow", "hundred_f")

# to get 3 ensemble's datasets of non-stationary & stationary
e3gam_df <- gamlss_mdl(osse_data, 3)
e3gum_df <- gumbel_mdl(osse_data, 3)
# write.csv(e3gam_df, "e3gam_df.csv")
# write.csv(e3gum_df, "e3gum_df.csv")

# evaluation of the above datasets
# print(evaluation(model, e3gam_df))
# print(evaluation(model, e3gum_df))


# error distribution in various ensembles
n <- 100
# error calculation
e1 <- eval_dist(n,  osse_data, 1, model)
e3 <- eval_dist(n,  osse_data, 3, model)
e6 <- eval_dist(n,  osse_data, 6, model)
e9 <- eval_dist(n,  osse_data, 9, model)
e12 <- eval_dist(n,  osse_data, 12, model)
e15 <- eval_dist(n,  osse_data, 15, model)
e30 <- eval_dist(n,  osse_data, 30, model)

# gamlss
e1gam <- unlist(e1[[1]])
e3gam <- unlist(e3[[1]])
e6gam <- unlist(e6[[1]])
e9gam <- unlist(e9[[1]])
e12gam <- unlist(e12[[1]])
e15gam <- unlist(e15[[1]])
e30gam <- unlist(e30[[1]])

# stationary
e1gum <- unlist(e1[[2]])
e3gum <- unlist(e3[[2]])
e6gum <- unlist(e6[[2]])
e9gum <- unlist(e9[[2]])
e12gum <- unlist(e12[[2]])
e15gum <- unlist(e15[[2]])
e30gum <- unlist(e30[[2]])


# error average to compare
gam_error <- cbind(e1gam, e3gam, e6gam, e9gam, e12gam, e15gam, e30gam)
gam_avg <- round(colMeans(gam_error), digits = 5)
gam_std <- round(apply(gam_error, 2, sd), digits = 5)

gum_error <- cbind(e1gum, e3gum, e6gum, e9gum, e12gum, e15gum, e30gum)
gum_avg <- round(colMeans(gum_error), digits = 5)
gum_std <- round(apply(gum_error, 2, sd), digits = 5)

cat("Non-stationary Error: ", gam_avg, "\n")
cat("Stationary Error: ", gum_avg, "\n")
cat("Non-stationary Standard Deviation: ", gam_std, "\n")
cat("Stationary Standard Deviation: ", gum_std, "\n")


# error distribution plot
# nonstationary
plot(density(e1gam), xlim = c(min(e1gum), max(e1gum)), ylim = c(0, 17), main="Error distribution of non-stationary (GAMLSS) & stationary estimations (n=100)", xlab="Error value", ylab="Density", col=1, lwd=2)
lines(density(e3gam), col=2, lwd=2)
lines(density(e1gum), col=3, lwd=2)
lines(density(e3gum), col=4, lwd=2)
lines(density(e6gum), col=5, lwd=2)
lines(density(e9gum), col=6, lwd=2)
lines(density(e12gum), col=7, lwd=2)
lines(density(e15gum), col=8, lwd=2)
legend("topright", legend = c("gamlss_e1", "gamlss_e3", "stationary_e1", "stationary_e3", "stationary_e6", "stationary_e9", "stationary_e12", "stationary_e15"), lty=1, lwd = 2, col=1:8)

# only GAMLSS at Chikugo
plot(density(e1gam), xlim = c(min(e3gam), max(e1gam)), ylim = c(0, 32), main="Error distribution of non-stationary (GAMLSS) estimations (n=100)", xlab="Error value", ylab="Density", col=2, lwd=2)
lines(density(e3gam), col=3, lwd=2)
lines(density(e6gam), col=4, lwd=2)
lines(density(e9gam), col=5, lwd=2)
lines(density(e12gam), col=6, lwd=2)
lines(density(e15gam), col=7, lwd=2)
lines(density(e30gam), col=8, lwd=2)
legend("topright", legend = c("gamlss_e1", "gamlss_e3", "gamlss_e6", "gamlss_e9", "gamlss_e12", "gamlss_e15", "gamlss_e30"), lty=1, lwd = 2, col=2:8)
