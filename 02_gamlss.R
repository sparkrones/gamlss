source("/work/a06/stakahashi/workspace/00_func.R")


# 1. argument settings
option_list <- list(
  make_option(c("--data"), type = "character", help="File path to read data", metavar="DATA"),
)

opt_parser <- OptionParser(usage="Usage: %prog --data=DATA", option_list=option_list)
opt <- parse_args(opt_parser)

if (is.null(opt$data)) {
  stop("Please specify the file path using --data option.")
}


# 2. import data
data <- readBin(opt$data, numeric(), size = 4, n = 240, endian = "little")

amax_list <- list()
for (i in seq(1, length(data), by = 2)) {
  amax_list[[length(amax_list) + 1]] <- c(data[i], data[i + 1])
}

amax <- do.call(rbind.data.frame, amax_list)
colnames(amax) <- c("year", "outflow")


# 3. outlier detection
fit_otlr <- fevd(outflow, amax, type = "Gumbel")
outlier <- as.vector(return.level(fit_otlr, return.period = 80000))
amax <- subset(amax, outflow <= outlier)


# 4. estimate 100-year flood benchmark by stationary gumbel distribution
# fitted to first 30 years amax
df_30 <- head(amax, 30)
fit <- fevd(outflow, df_30, type = "Gumbel")
# plot(fit)
f_1980s <- as.vector(return.level(fit, return.period = 100))

# plot stationary gumbel distribution
# plot(fit, type = "density", main = "Stationary Gumbel Distribution (1980-2009)", lwd = 2)
# abline(v = f_1980s, col = "red", lty = 2, lwd =3)


# 5. GAMLSS fitting (120 years)
model <- gamlss(outflow ~ year, mu.fo = ~ year, sigma.fo = ~ year, family = "GU", data = amax)
mu <- lpred(model, what = "mu", type = "response")
sigma <- lpred(model, what = "sigma", type = "response")

# non-exceedance probability of 100-year flood estimated above in each year
nonx_prob <- pGU(f_1980s, mu = mu, sigma = sigma)
# return period corresponding to non-exceedance prob in each year
r <- 1 / (1 - nonx_prob)
  
# 100-year flood magnitudes
hundred_f <- qGU(0.99, mu = mu, sigma = sigma)

df <- data.frame(amax, hundred_f, r, nonx_prob)


# plot the 100-year flood magnitude with amax
plot(df$year, df$outflow, type = "p", pch = 16, col = "blue", xlab = "year", ylab = "annual max outflow [m^3/s]", main = "Nonstationary 100-year Flood Magnitudes (1980-2099)")
lines(df$year, df$hundred_f, col = "red", lty = 1, lwd = 3)

# return period
# plot(df$year, df$r, type = 'l', col = "red", lty = 1, lwd = 3, xlab = "year", ylab = "return period [years]", main = "Return Period (1980-2099)")
