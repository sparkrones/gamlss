source("C:/Users/cherr/Dropbox/VS Code/research/RA_GH/01_func.R") # change path to your own

# 1. import data
path <- "C:/Users/cherr/Dropbox/VS Code/research/RA_GH/data/max_y120.bin" # change path to your own
data <- readBin(path, numeric(), size = 4, n = 240, endian = "little")

amax_list <- list()
for (i in seq(1, length(data), by = 2)) {
  amax_list[[length(amax_list) + 1]] <- c(data[i], data[i + 1])
}

amax <- do.call(rbind.data.frame, amax_list)
colnames(amax) <- c("year", "outflow")


# 2. estimate 100-year flood benchmark by stationary gumbel distribution
# fitted to first 30 years amax
df_30 <- head(amax, 30)
fit <- fevd(outflow, df_30, type = "Gumbel")
# plot(fit)
f_1980s <- as.vector(return.level(fit, return.period = 100))
# print(f_1980s) # 1702.702

# plot stationary gumbel distribution
# plot(fit, type = "density", main = "Stationary Gumbel Distribution (1980-2009)", lwd = 2)
# abline(v = f_1980s, col = "red", lty = 2, lwd =3)


# 3. GAMLSS fitting (120 years)
model <- gamlss(outflow ~ year, mu.fo = ~ year, sigma.fo = ~ year, family = "GU", data = amax)
mu <- lpred(model, what = "mu", type = "response")
sigma <- lpred(model, what = "sigma", type = "response")

# non-exceedance probability of 100-year flood estimated above in each year
nonx_prob <- pGU(f_1980s, mu = mu, sigma = sigma)

# return period corresponding to non-exceedance prob in each year
r <- 1 / (1 - nonx_prob)

# 100-year flood magnitudes
hundred_f <- qGU(0.99, mu = mu, sigma = sigma)
# print(hundred_f)


df <- data.frame(amax, hundred_f, r, nonx_prob)
# write.csv(df, "df.csv")