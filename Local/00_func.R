library(gamlss)
library(gamlss.dist)
library(gamlss.data)
library(extRemes)
library(tidyverse)
library(dplyr)
library(optparse)
library(gridExtra)
library(gt)
library(grid)
library(gridExtra)


# gamlss
gamlss_func <- function(path) {
  # 1. read and prepare data
  data <- readBin(path, numeric(), size = 4, n = 240, endian = "little")

  amax_list <- list()
  for (i in seq(1, length(data), by = 2)) {
    amax_list[[length(amax_list) + 1]] <- c(data[i], data[i + 1])
  }

  amax <- do.call(rbind.data.frame, amax_list)
  colnames(amax) <- c("year", "outflow")


  # 2. outlier removal
  fit_otlr <- fevd(amax$outflow, amax, type = "Gumbel")
  outlier <- as.vector(return.level(fit_otlr, return.period = 20000))
  amax <- subset(amax, amax$outflow <= outlier)


  # 3. estimate 100-year flood benchmark by stationary gumbel distribution
  # fitted to first 30 years amax
  df_30 <- head(amax, 30)
  fit <- fevd(df_30$outflow, type = "Gumbel")
  # plot(fit)
  f_1980s <- as.vector(return.level(fit, return.period = 100))

  # plot stationary gumbel distribution
  # plot(fit, type = "density", main = "Stationary Gumbel Distribution (1980-2009)", lwd = 2)
  # abline(v = f_1980s, col = "red", lty = 2, lwd =3)


  # 4. GAMLSS fitting (120 years)
  model <- gamlss(outflow ~ year, mu.fo = ~ year, sigma.fo = ~ year, family = "GU", data = amax)
  mu <- lpred(model, what = "mu", type = "response")
  sigma <- lpred(model, what = "sigma", type = "response")

  # non-exceedance probability of 100-year flood (f_1980s) in each year
  nonx_prob <- pGU(f_1980s, mu = mu, sigma = sigma)
  # return period corresponding to non-exceedance prob in each year
  r <- 1 / (1 - nonx_prob)
    
  # 100-year flood magnitudes
  hundred_f <- qGU(0.99, mu = mu, sigma = sigma)


  # 6. set the minimum limit from 2-year and 3-year outflow
  # truth 2-year/3-year outflow
  q2 <- qGU((1 - 1/2), mu = mu, sigma = sigma)
  q3 <- qGU((1 - 1/3), mu = mu, sigma = sigma)
  # min limit
  min_lmt <- q2[[1]] - (q3[[1]] - q2[[1]]) * 1.5

  # if min_lmt is less than 0, return the error message and stop the operation
  if (min_lmt < 0) {
    stop("Error: min_lmt is less than 0. Operation cannot continue.")
  }

  return(list(model = model, mu = mu, sigma = sigma, year = amax$year, outflow = amax$outflow, min_lmt = min_lmt, q2 = q2, q3 = q3, hundred_f = hundred_f, nonx_prob = nonx_prob, r = r))
}


# gamlss - mu & sigma: stationary
gamlss_stat <- function(path) {
  # 1. read and prepare data
  data <- readBin(path, numeric(), size = 4, n = 240, endian = "little")

  amax_list <- list()
  for (i in seq(1, length(data), by = 2)) {
    amax_list[[length(amax_list) + 1]] <- c(data[i], data[i + 1])
  }

  amax <- do.call(rbind.data.frame, amax_list)
  colnames(amax) <- c("year", "outflow")


  # 2. outlier removal
  fit_otlr <- fevd(amax$outflow, amax, type = "Gumbel")
  outlier <- as.vector(return.level(fit_otlr, return.period = 20000))
  amax <- subset(amax, amax$outflow <= outlier)


  # 3. estimate 100-year flood benchmark by stationary gumbel distribution
  # fitted to first 30 years amax
  df_30 <- head(amax, 30)
  fit <- fevd(df_30$outflow, type = "Gumbel")
  f_1980s <- as.vector(return.level(fit, return.period = 100))


  # 4. GAMLSS fitting (120 years)
  model <- gamlss(outflow ~ 1, mu.fo = ~1, sigma.fo = ~1, family = "GU", data = amax)
  mu <- lpred(model, what = "mu", type = "response")
  sigma <- lpred(model, what = "sigma", type = "response")

  # non-exceedance probability of 100-year flood (f_1980s) in each year
  nonx_prob <- pGU(f_1980s, mu = mu, sigma = sigma)

  # return period corresponding to non-exceedance prob in each year
  r <- 1 / (1 - nonx_prob)
    
  # 100-year flood magnitudes
  hundred_f <- qGU(0.99, mu = mu, sigma = sigma)


  # 6. set the minimum limit from 2-year and 3-year outflow
  # truth 2-year/3-year outflow
  q2 <- qGU((1 - 1/2), mu = mu, sigma = sigma)
  q3 <- qGU((1 - 1/3), mu = mu, sigma = sigma)
  # min limit
  min_lmt <- q2[[1]] - (q3[[1]] - q2[[1]]) * 1.5

  # if min_lmt is less than 0, return the error message and stop the operation
  if (min_lmt < 0) {
    stop("Error: min_lmt is less than 0. Operation cannot continue.")
  }

  return(list(model = model, mu = mu, sigma = sigma, year = amax$year, outflow = amax$outflow, q2 = q2, q3 = q3, min_lmt = min_lmt, hundred_f = hundred_f, nonx_prob = nonx_prob, r = r))
}

# gamlss - mu (mean): stationary & sigma (variance): non-statinary
gamlss_mean_stat <- function(path) {
  # 1. read and prepare data
  data <- readBin(path, numeric(), size = 4, n = 240, endian = "little")

  amax_list <- list()
  for (i in seq(1, length(data), by = 2)) {
    amax_list[[length(amax_list) + 1]] <- c(data[i], data[i + 1])
  }

  amax <- do.call(rbind.data.frame, amax_list)
  colnames(amax) <- c("year", "outflow")


  # 2. outlier removal
  fit_otlr <- fevd(amax$outflow, amax, type = "Gumbel")
  outlier <- as.vector(return.level(fit_otlr, return.period = 20000))
  amax <- subset(amax, amax$outflow <= outlier)


  # 3. estimate 100-year flood benchmark by stationary gumbel distribution
  # fitted to first 30 years amax
  df_30 <- head(amax, 30)
  fit <- fevd(df_30$outflow, type = "Gumbel")
  f_1980s <- as.vector(return.level(fit, return.period = 100))


  # 4. GAMLSS fitting (120 years)
  model <- gamlss(outflow ~ 1, mu.fo = ~ 1, sigma.fo = ~ year, family = "GU", data = amax)
  mu <- lpred(model, what = "mu", type = "response")
  sigma <- lpred(model, what = "sigma", type = "response")

  # non-exceedance probability of 100-year flood (f_1980s) in each year
  nonx_prob <- pGU(f_1980s, mu = mu, sigma = sigma)

  # return period corresponding to non-exceedance prob in each year
  r <- 1 / (1 - nonx_prob)
    
  # 100-year flood magnitudes
  hundred_f <- qGU(0.99, mu = mu, sigma = sigma)


  # 6. set the minimum limit from 2-year and 3-year outflow
  # truth 2-year/3-year outflow
  q2 <- qGU((1 - 1/2), mu = mu, sigma = sigma)
  q3 <- qGU((1 - 1/3), mu = mu, sigma = sigma)
  # min limit
  min_lmt <- q2[[1]] - (q3[[1]] - q2[[1]]) * 1.5

  # if min_lmt is less than 0, return the error message and stop the operation
  if (min_lmt < 0) {
    stop("Error: min_lmt is less than 0. Operation cannot continue.")
  }

  return(list(model = model, mu = mu, sigma = sigma, year = amax$year, outflow = amax$outflow, q2 = q2, q3 = q3, min_lmt = min_lmt, hundred_f = hundred_f, nonx_prob = nonx_prob, r = r))
}


osse_min_stat <- function(path) {
  # produce GAMLSS variables
  gamlss <- gamlss_stat(path)
  mu <- gamlss$mu
  sigma <- gamlss$sigma
  df <- data.frame(year = gamlss$year, outflow = gamlss$outflow)
  min_lmt <- gamlss$min_lmt
  q2 <- gamlss$q2

  # OSSE generation
  y <- rGU(nrow(df), mu, sigma)
  n_df <- cbind(data.frame(df$year, y))
  colnames(n_df) <- c("year", "outflow")
  
  # fit newly generated data to gamlss
  n_model <- gamlss(outflow ~ 1, mu.fo = ~ 1, sigma.fo = ~ 1, family = "GU", data = n_df)
  n_mu <- lpred(n_model, what = "mu", type = "response")
  n_sigma <- lpred(n_model, what = "sigma", type = "response")
  
  for (i in seq_along(y)) {
    nonx <- pGU(y, mu = n_mu, sigma = n_sigma)
    rp <- 1 / (1 - nonx)
    # truth 2-year/3-year outflow
    # q2 <- qGU((1 - 1/2), mu = mu, sigma = sigma)
    # q3 <- qGU((1 - 1/3), mu = mu, sigma = sigma)
    # osse 2-year/3-year outflow
    osse_q2 <- qGU((1 - 1/2), mu = n_mu, sigma = n_sigma)
    osse_q3 <- qGU((1 - 1/3), mu = n_mu, sigma = n_sigma)

    # negative value complements
    if (nonx[[i]] < 0.5) {
      n_df$outflow[[i]] <- min_lmt + (q2[[i]] - min_lmt) * (rp[[i]] - 1)
    }
  }
  
  return(list(outflow = n_df$outflow, osse2 = osse_q2, osse3 = osse_q3))
}

osse_min_mean <- function(path) {
  # produce GAMLSS variables
  gamlss <- gamlss_mean_stat(path)
  mu <- gamlss$mu
  sigma <- gamlss$sigma
  df <- data.frame(year = gamlss$year, outflow = gamlss$outflow)
  min_lmt <- gamlss$min_lmt
  q2 <- gamlss$q2

  # OSSE generation
  y <- rGU(nrow(df), mu, sigma)
  n_df <- cbind(data.frame(df$year, y))
  colnames(n_df) <- c("year", "outflow")
  
  # fit newly generated data to gamlss
  n_model <- gamlss(outflow ~ 1, mu.fo = ~ 1, sigma.fo = ~ year, family = "GU", data = n_df)
  n_mu <- lpred(n_model, what = "mu", type = "response")
  n_sigma <- lpred(n_model, what = "sigma", type = "response")
  
  for (i in seq_along(y)) {
    nonx <- pGU(y, mu = n_mu, sigma = n_sigma)
    rp <- 1 / (1 - nonx)
    # osse 2-year/3-year outflow
    osse_q2 <- qGU((1 - 1/2), mu = n_mu, sigma = n_sigma)
    osse_q3 <- qGU((1 - 1/3), mu = n_mu, sigma = n_sigma)

    # negative value complements
    if (nonx[[i]] < 0.5) {
      n_df$outflow[[i]] <- min_lmt + (q2[[i]] - min_lmt) * (rp[[i]] - 1)
    }
  }
  
  return(list(outflow = n_df$outflow, osse2 = osse_q2, osse3 = osse_q3))
}


# general
# random generation from GAMLSS fitted to 120 years AMAX
osse_min <- function(path) {
  # produce GAMLSS variables
  gamlss <- gamlss_func(path)
  mu <- gamlss$mu
  sigma <- gamlss$sigma
  df <- data.frame(year = gamlss$year, outflow = gamlss$outflow)
  min_lmt <- gamlss$min_lmt
  q2 <- gamlss$q2

  # OSSE generation
  y <- rGU(nrow(df), mu, sigma)
  n_df <- cbind(data.frame(df$year, y))
  colnames(n_df) <- c("year", "outflow")
  
  # fit newly generated data to gamlss
  n_model <- gamlss(outflow ~ year, mu.fo = ~ year, sigma.fo = ~ year, family = "GU", data = n_df)
  n_mu <- lpred(n_model, what = "mu", type = "response")
  n_sigma <- lpred(n_model, what = "sigma", type = "response")
  
  for (i in seq_along(y)) {
    nonx <- pGU(y, mu = n_mu, sigma = n_sigma)
    rp <- 1 / (1 - nonx)
    # truth 2-year/3-year outflow
    # q2 <- qGU((1 - 1/2), mu = mu, sigma = sigma)
    # q3 <- qGU((1 - 1/3), mu = mu, sigma = sigma)
    # osse 2-year/3-year outflow
    osse_q2 <- qGU((1 - 1/2), mu = n_mu, sigma = n_sigma)
    osse_q3 <- qGU((1 - 1/3), mu = n_mu, sigma = n_sigma)

    # negative value complements
    if (nonx[[i]] < 0.5) {
      n_df$outflow[[i]] <- min_lmt + (q2[[i]] - min_lmt) * (rp[[i]] - 1)
    }
  }
  
  return(list(outflow = n_df$outflow, osse2 = osse_q2, osse3 = osse_q3))
}

# OSSE generation in specified number
# osse(file path to read, number of OSSE to generate)
osse <- function(path, osse_num) {
  osse_list <- list()

  # generate OSSE from data designated at "path" in 02_gamlss.R
  for (i in 1:osse_num) {
    # choose which osse generation method to use
    # generated <- osse_min(path)   # non-stationary
    generated <- osse_min_stat(path)   # stationary mean and variance
    # generated <- osse_min_mean(path)   # stationary mean and non-stationary variance
    osse_list[[paste0("outflow_", i)]] <- generated$outflow
    osse_list[[paste0("q2_", i)]] <- generated$osse2
    osse_list[[paste0("q3_", i)]] <- generated$osse3
  }

  return(osse_list)
}


# nonstationary GAMLSS
# gamlss_mdl(osse_data, the_number_of_ensembles)
gamlss_mdl <- function(data, e) {
  # extract dataset in the number of ensembles
  n_osse <- length(data)
  index <- sample(1:n_osse, e, replace = FALSE)
  y_120 <- data[index]
  
  # reshape 120*3 -> 360*1
  outflow <- c(t(y_120))
  
  # adjust the length of years to the number of osse
  n <- seq(0, (length(outflow)-1))
  year <- 1981 + n * 1/e
  
  df <- cbind(data.frame(year, outflow))
  
  
  # gamlss fitting
  model <- gamlss(outflow ~ year, mu.fo = ~ year, sigma.fo = ~ year, family = "GU", data = df)
  mu <- lpred(model, what = "mu", type = "response")
  sigma <- lpred(model, what = "sigma", type = "response")

  # the 100-year flood estimation
  syt_hundred = qGU(0.99, mu = mu, sigma = sigma)
  
  # dataframe of osse
  e_df <- cbind(data.frame(df, syt_hundred))
  colnames(e_df) <- c("year", "outflow", "hundred_f")
  
  return(e_df)
}

gamlss_stat_mdl <- function(data, e) {
  # extract dataset in the number of ensembles
  n_osse <- length(data)
  index <- sample(1:n_osse, e, replace = FALSE)
  y_120 <- data[index]
  
  # reshape 120*3 -> 360*1
  outflow <- c(t(y_120))
  
  # adjust the length of years to the number of osse
  n <- seq(0, (length(outflow)-1))
  year <- 1981 + n * 1/e
  
  df <- cbind(data.frame(year, outflow))
  
  
  # gamlss fitting
  model <- gamlss(outflow ~ 1, mu.fo = ~ 1, sigma.fo = ~ 1, family = "GU", data = df)
  mu <- lpred(model, what = "mu", type = "response")
  sigma <- lpred(model, what = "sigma", type = "response")

  # the 100-year flood estimation
  syt_hundred = qGU(0.99, mu = mu, sigma = sigma)
  
  # dataframe of osse
  e_df <- cbind(data.frame(df, syt_hundred))
  colnames(e_df) <- c("year", "outflow", "hundred_f")
  
  return(e_df)
}

gamlss_mean_mdl <- function(data, e) {
  # extract dataset in the number of ensembles
  n_osse <- length(data)
  index <- sample(1:n_osse, e, replace = FALSE)
  y_120 <- data[index]
  
  # reshape 120*3 -> 360*1
  outflow <- c(t(y_120))
  
  # adjust the length of years to the number of osse
  n <- seq(0, (length(outflow)-1))
  year <- 1981 + n * 1/e
  
  df <- cbind(data.frame(year, outflow))
  
  
  # gamlss fitting
  model <- gamlss(outflow ~ 1, mu.fo = ~ 1, sigma.fo = ~ year, family = "GU", data = df)
  mu <- lpred(model, what = "mu", type = "response")
  sigma <- lpred(model, what = "sigma", type = "response")

  # the 100-year flood estimation
  syt_hundred = qGU(0.99, mu = mu, sigma = sigma)
  
  # dataframe of osse
  e_df <- cbind(data.frame(df, syt_hundred))
  colnames(e_df) <- c("year", "outflow", "hundred_f")
  
  return(e_df)
}


# stationary gumbel distribution (extRemes)
# gumbel_mdl(osse_data, the_number_of_ensembles)
gumbel_mdl <- function(data, e) {
  # extract dataset in the number of ensembles
  n_osse <- length(data)
  index <- sample(1:n_osse, e, replace = FALSE)
  y_120 <- data[index]

  # reshape
  outflow <- c(t(y_120))
  
  # adjust the length of years to the number of osse
  n <- seq(0, (length(outflow)-1))
  year <- 1980 + n * 1/e
  
  df <- cbind(data.frame(year, outflow))
  # extract last 30-year dataset
  df_30 <- tail(df, 30 * e)


  # stationary (gumbel)
  # extract last 30 years' amax
  amax_30 <- df_30$outflow
  fit <- fevd(amax_30, type = "Gumbel")
  f_pred <- erlevd(fit, period = 100)
  
  e_df <- cbind(data.frame(df_30, f_pred))
  colnames(e_df) <- c("year", "outflow", "hundred_f")

  return(e_df)
}


# evaluation(gamlss_func(path), gamlss e_df, gumbel (stationary) e_df, the number of ensembles)
# basically used in eval_dist()
evaluation <- function(gamlss, gam_df, gum_df, e) {
  # the mean of the 100-year flood of last 30 years
  # error = estimated / truth - 1
  gam_30 <- mean(tail(gam_df$hundred_f, 30 * e))
  gum_30 <- gum_df$hundred_f[1]
  truth_30 <- mean(tail(gamlss$hundred_f, 30))
  # truth_30 <- mean(tail(truth, 30 * e))

  gam_error <- (gam_30 / truth_30) - 1
  gum_error <- (gum_30 / truth_30) - 1

  return(list(gam = gam_error, gum = gum_error))
}


# eval_dist(the_number_of_experiments, data_parameter_of_mdl, e_parameter_of_mdl, gamlss_func(path))
eval_dist <- function(n, data, e, gamlss) {
  gam_list <- list()
  gum_list <- list()

  for (i in 1:n) {
    gam_df <- gamlss_mdl(data, e)
    # gam_df <- gamlss_stat_mdl(data, e)
    # gam_df <- gamlss_mean_mdl(data, e)
    gum_df <- gumbel_mdl(data, e)
    result <- evaluation(gamlss, gam_df, gum_df, e)

    gam_list[[i]] <- result$gam
    gum_list[[i]] <- result$gum
  }

  return(list(gam_list, gum_list))
}
