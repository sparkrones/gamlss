source("C:/Users/cherr/Dropbox/VS Code/research/RA_GH/01_func.R")
source("C:/Users/cherr/Dropbox/VS Code/research/RA_GH/02_gamlss.R")

# import osse generation data
osse_data <- read.csv("C:/Users/cherr/Dropbox/VS Code/research/RA_GH/data/osse_df.csv")


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
e5 <- eval_dist(n,  osse_data, 5, model)
e10 <- eval_dist(n,  osse_data, 10, model)
e12 <- eval_dist(n,  osse_data, 12, model)
e30 <- eval_dist(n,  osse_data, 30, model)
e60 <- eval_dist(n,  osse_data, 60, model)
e90 <- eval_dist(n,  osse_data, 90, model)
e100 <- eval_dist(n,  osse_data, 100, model)

# gamlss
e1gam <- unlist(e1[[1]])
e3gam <- unlist(e3[[1]])
e5gam <- unlist(e5[[1]])
e10gam <- unlist(e10[[1]])
e12gam <- unlist(e12[[1]])
e30gam <- unlist(e30[[1]])
e60gam <- unlist(e60[[1]])
e90gam <- unlist(e90[[1]])
e100gam <- unlist(e100[[1]])

# stationary
e1gum <- unlist(e1[[2]])
e3gum <- unlist(e3[[2]])
e5gum <- unlist(e5[[2]])
e10gum <- unlist(e10[[2]])
e12gum <- unlist(e12[[2]])
e30gum <- unlist(e30[[2]])
e60gum <- unlist(e60[[2]])
e90gum <- unlist(e90[[2]])
e100gum <- unlist(e100[[2]])


# error average to compare
gam_error <- cbind(e1gam, e3gam, e5gam, e10gam, e12gam, e30gam, e60gam, e90gam, e100gam)
gam_avg <- round(colMeans(gam_error), digits = 5)
gam_std <- apply(gam_error, 2, sd)

gum_error <- cbind(e1gum, e3gum, e5gum, e10gum, e12gum, e30gum, e60gum, e90gum, e100gum)
gum_avg <- round(colMeans(gum_error), digits = 5)
gum_std <- apply(gum_error, 2, sd)

cat("Non-stationary Error: ", gam_avg, "\n")
cat("Stationary Error: ", gum_avg, "\n")


# error distribution plot
# nonstationary
plot(density(e1gam), xlim = c(min(e1gam), max(e1gam)), ylim = c(0, 55), main="Error distribution of non-stationary (GAMLSS) estimations (n=100)", xlab="Error value", ylab="Density", col=1, lwd=2)
lines(density(e3gam), col=2, lwd=2)
lines(density(e5gam), col=3, lwd=2)
lines(density(e10gam), col=4, lwd=2)
lines(density(e30gam), col=5, lwd=2)
lines(density(e60gam), col=6, lwd=2)
lines(density(e90gam), col=7, lwd=2)
lines(density(e100gam), col=8, lwd=2)
legend("topright", legend = c("e1", "e3", "e5", "e10", "e30", "e60", "e90", "e100"), lty=1, lwd = 2, col=1:8)

# stationary
plot(density(e1gum), xlim = c(min(e1gum), max(e1gum)), ylim = c(0, 25), main="Error distribution of stationary estimations (n=100)", xlab="Error value", ylab="Density", col=1, lwd=2)
lines(density(e3gum), col=2, lwd=2)
lines(density(e5gum), col=3, lwd=2)
lines(density(e10gum), col=4, lwd=2)
lines(density(e30gum), col=5, lwd=2)
lines(density(e60gum), col=6, lwd=2)
lines(density(e90gum), col=7, lwd=2)
lines(density(e100gum), col=8, lwd=2)
legend("topright", legend = c("e1", "e3", "e5", "e10", "e30", "e60", "e90", "e100"), lty=1, lwd = 2, col=1:8)
