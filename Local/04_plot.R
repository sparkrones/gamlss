source("C:/Users/cherr/Dropbox/VS Code/research/RA_GH/03_evaluation.R")


# plot results
# set the canvas layout
saved_name <- file.path(paste0("river", "ssp", ".png"))
png(saved_name, width = 1200, height = 900)

dev.new(width = 12, height = 9)
layout_matrix <- matrix(c(1, 2, 3, 4), ncol = 2, byrow = TRUE)
layout(layout_matrix, widths = c(3, 1), heights = c(1, 1))

# top-center: river name
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
grid.text("River Info", x = 0.5, y = 0.95, just = "center", gp = gpar(fontsize = 12, fontface = "bold"))
popViewport()


# top-left: non-stationary 100-year flood estimates with amax
par(mar = c(5, 5, 4, 2))
plot(gamlss_df$year, gamlss_df$outflow, type = "p", pch = 16, col = "blue", xlab = "year", ylab = "annual max outflow [m^3/s]", main = "Nonstationary 100-year Flood Estimates (1981-2100)")
lines(gamlss_df$year, gamlss_df$hundred_f, col = "red", lty = 1, lwd = 3)
legend("topleft", legend = c("100-year flood magnitude", "original annual max outflow"), col = c("red", "blue"), pch = c(NA, 16), lwd = c(2, NA))

# return period
# plot(df$year, df$r, type = 'l', col = "red", lty = 1, lwd = 3, xlab = "year", ylab = "return period [years]", main = "Return Period (1980-2099)")


# bottom-left: river info
par(mar = c(3, 3, 2, 1))
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


# middle-right: table of the average of error
plot.new()
text(error)
grid.text("Average error value (n=100)", y = 0.9, gp = gpar(fontsize = 12, fontface = "bold"))
grid.draw(tableGrob(error, rows = NULL))
popViewport()


# bottom-right: 
textplot(std)
pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 2))
grid.text("Average standard deviation (n=100)", y = 0.9, just = "center", gp = gpar(fontsize = 12, fontface = "bold"))
grid.draw(tableGrob(std, rows = NULL))
popViewport()


# top-center: main title
mtext("river", side = 3, line = -2, cex = 1.2)


# save the entire plot to the PNG file
dev.off()