source("C:/Users/cherr/Dropbox/VS Code/research/RA_GH/03_evaluation.R")


# plot results
# set the canvas layout
saved_name <- file.path(paste0("river", "ssp", ".png"))
png(saved_name, width = 1600, height = 1200)
par(mfrow = c(2, 2))

# top-left: non-stationary 100-year flood estimates with amax
plot(gamlss_df$year, gamlss_df$outflow, type = "p", pch = 16, col = "blue", xlab = "year", ylab = "annual max outflow [m^3/s]", main = "Nonstationary 100-year Flood Estimates (1981-2100)")
lines(gamlss_df$year, gamlss_df$hundred_f, col = "red", lty = 1, lwd = 3)
legend("topleft", legend = c("100-year flood magnitude", "original annual max outflow"), col = c("red", "blue"), pch = c(NA, 16), lwd = c(2, NA))

# return period
# plot(df$year, df$r, type = 'l', col = "red", lty = 1, lwd = 3, xlab = "year", ylab = "return period [years]", main = "Return Period (1980-2099)")


# top-right: error distribution
color_list <- c("#FF7F50", "#FF3399", "#191970", "#0000FF", "#00BFFF", "#00FFFF", "#00CED1", "#3CB371")
# nonstationary
plot(density(e1gam), xlim = c(min(e3gam), max(e1gum)), ylim = c(0, max(density(e12gum)$y)), main="Error distribution (n=100)", xlab="Error value", ylab="Density", col=color_list[1], lwd=2)
lines(density(e3gam), col=color_list[2], lwd=2)
lines(density(e1gum), col=color_list[3], lwd=2)
lines(density(e3gum), col=color_list[4], lwd=2)
lines(density(e6gum), col=color_list[5], lwd=2)
lines(density(e9gum), col=color_list[6], lwd=2)
lines(density(e12gum), col=color_list[7], lwd=2)
# lines(density(e15gum), col=color_list[8], lwd=2)
legend("topright", legend = c("gamlss_e1", "gamlss_e3", "stationary_e1", "stationary_e3", "stationary_e6", "stationary_e9", "stationary_e12"), lty=1, lwd = 2, col=color_list[1:7])


# bottom-left: river info
text()


# bottom-right: tables of the average of error and standard deviation
# titles
texts <- "                Average error value (n=100)                  Average standard deviation (n=100)"
titles <- textGrob(texts, gp = gpar(fontsize = 12, fontface = "bold"))

# write tables in a grid
error_table <- tableGrob(error)
std_table <- tableGrob(std)
combined_grid <- grid.arrange(error_table, std_table, ncol = 2, top = titles)


# top-center: main title
mtext("river", side = 3, line = -2, cex = 1.2)