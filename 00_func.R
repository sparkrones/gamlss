# install packages
# remove # or comment-out if necessary

# install.packages("gamlss")
# install.packages("gamlss.dist")
# install.packages("gamlss.data")
# install.packages("extRemes")
# install.packages("tidyverse")
# install.packages("dplyr")

library(gamlss)
library(gamlss.dist)
library(gamlss.data)
library(extRemes)
library(tidyverse)
library(dplyr)


# 03_osse2csv.R: random generation from GAMLSS fitted to 120 years AMAX
osse <- function(mu, sigma) {
  y <- rGU(120, mu, sigma)
  
  for (i in seq_along(y)) {
    nonx[i] <- pGU(y[i], mu = mu[i], sigma = sigma[i])
    rp[i] <- 1 / (1 - nonx[i])
    
    if (nonx[i] < 0.5) {
      q2[i] <- qGU((1 - 1/2), mu = mu[i], sigma = sigma[i])
      q3[i] <- qGU((1 - 1/3), mu = mu[i], sigma = sigma[i])
      
      y[i] <- (q3[i] - q2[i]) * rp[i]  + 3 * q2[i] - 2 * q3[i]
    }
  }
  return(y)
}


# nonstationary GAMLSS
# gamlss_mdl(osse_data, the_number_of_ensembles)
gamlss_mdl <- function(data, e) {
  # extract dataset in the number of ensembles
  n_osse <- length(data)
  index <- sample(1:n_osse, e, replace = FALSE)
  y_120 <- data[index, ]
  
  # reshape 120*3 -> 360*1
  outflow <- y_120 %>% 
    gather(value = "Value") %>%
    pull()
  
  # adjust the length of years to the number of osse
  n <- seq(0, (length(outflow)-1))
  year <- 1980 + n * 1/e
  
  df <- cbind(data.frame(year, outflow))
  
  
  # gamlss
  model <- gamlss(outflow ~ year, mu.fo = ~year, sigma.fo = ~ year, family = "GU", data = df)
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
  y_120 <- data[index, ]

  # reshape
  outflow <- y_120 %>% 
    gather(value = "Value") %>%
    pull()
  
  # adjust the length of years to the number of osse
  n <- seq(0, (length(outflow)-1))
  year <- 1980 + n * 1/e
  
  df <- cbind(data.frame(year, outflow))
  df_30 <- tail(df, 30 * e)


  # stationary (gumbel)
  # extract last 30 years amax
  amax_30 <- df_30$outflow
  fit <- fevd(amax_30, type = "Gumbel")
  f_pred <- erlevd(fit, period = 100)
  
  e_df <- cbind(data.frame(df_30, f_pred))
  colnames(e_df) <- c("year", "outflow", "hundred_f")

  return(e_df)
}


# evaluation: RMSE (Root Mean Squared Error), MAE (Mean Absolute Error)
# evaluation(truth model, gamlss e_df, gumbel (stationary) e_df, the number of ensembles)
# basically used in eval_dist()
evaluation <- function(model, gam_df, gum_df, e) {
  # to get the truth values in the same time series with osse
  e_mu <- predict(model, what = "mu", newdata = data.frame(year = gam_df$year), type = "response")
  e_sigma <- predict(model, what = "sigma", newdata = data.frame(year = gam_df$year), type = "response")
  truth <- qGU(0.99, mu = e_mu, sigma = e_sigma)

  # the mean of the 100-year flood of last 30 years
  # error = estimated / truth - 1
  gam_30 <- mean(tail(gam_df$hundred_f, 30 * e))
  gum_30 <- mean(tail(gum_df$hundred_f, 30 * e))
  truth_30 <- mean(tail(truth, 30 * e))

  gam_error <- (gam_30 / truth_30) - 1
  gum_error <- (gum_30 / truth_30) - 1

  return(list(gam = gam_error, gum = gum_error))
}


# RMSE/MAE distribution of GAMLSS
# eval_dist(the_number_of_experiments, data_parameter_of_mdl, e_parameter_of_mdl, truth_model_parameter_of_evaluation)
eval_dist <- function(n, data, e, model) {
  gam_list <- list()
  gum_list <- list()

  for (i in 1:n) {
    gam_df <- gamlss_mdl(data, e)
    gum_df <- gumbel_mdl(data, e)
    result <- evaluation(model, gam_df, gum_df, e)

    gam_list[[i]] <- result$gam
    gum_list[[i]] <- result$gum
  }

  return(list(gam_list, gum_list))
}


# river stats
