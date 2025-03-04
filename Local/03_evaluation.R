source("C:/Users/cherr/Dropbox/VS Code/research/RA_GH/00_func.R")

# 1. prep
# import osse generation data
osse_data <- read.csv("C:/Users/cherr/Dropbox/VS Code/research/RA_GH/data/shimanto_stat_osse.csv")

# calculate gamlss variables again
path <- "C:/Users/cherr/Dropbox/VS Code/research/RA_GH/data/shimanto_y120.bin"
gamlss_df <- gamlss_stat(path)


# error distribution in various ensembles
n <- 100
# 2. error calculation
e1 <- eval_dist(n,  osse_data, 1, gamlss_df)
e3 <- eval_dist(n,  osse_data, 3, gamlss_df)
e6 <- eval_dist(n,  osse_data, 6, gamlss_df)
e9 <- eval_dist(n,  osse_data, 9, gamlss_df)
e12 <- eval_dist(n,  osse_data, 12, gamlss_df)
e15 <- eval_dist(n,  osse_data, 15, gamlss_df)
e30 <- eval_dist(n,  osse_data, 30, gamlss_df)

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


# 3. error and standard deviation average (n) to compare
gam_error <- cbind(e1gam, e3gam, e6gam, e9gam, e12gam, e15gam, e30gam)
gam_avg <- round(colMeans(gam_error), digits = 5)
gam_std <- round(apply(gam_error, 2, sd), digits = 5)

gum_error <- cbind(e1gum, e3gum, e6gum, e9gum, e12gum, e15gum, e30gum)
gum_avg <- round(colMeans(gum_error), digits = 5)
gum_std <- round(apply(gum_error, 2, sd), digits = 5)

error <- data.frame(Non_stationary = c(gam_avg), Stationary = c(gum_avg))
std <- data.frame(Non_stationary = c(gam_std), Stationary = c(gum_std))

# set the row name
row_names <- paste0("e", c(1, 3, 6, 9, 12, 15, 30))
rownames(error) <- row_names
rownames(std) <- row_names


# 4. plot results
# set the canvas layout
saved_name <- file.path(paste0("river", "ssp", ".png"))
png(saved_name, width = 1200, height = 900) # two plots are not be drawn unless the pixel sizes are the same??

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


###NULLL
# titles
texts <- "                Average error value (n=100)                  Average standard deviation (n=100)"
titles <- textGrob(texts, gp = gpar(fontsize = 12, fontface = "bold"))

# write tables in a grid
error_table <- tableGrob(error, rows = NULL)

# bottom-right: table of the average of standard deviation
std_table <- tableGrob(std, rows = NULL)
combined_grid <- grid.arrange(error_table, std_table, ncol = 2, top = titles)


# top-center: main title
mtext("river", side = 3, line = -2, cex = 1.2)


# save the entire plot to the PNG file
dev.off()
