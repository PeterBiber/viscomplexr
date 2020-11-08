# ------------------------------------------------------------------------------
# Creating illustrations for the documentation on the website

# Aesthetics section


figPath <- "CreateFiguresForWebsiteDocs/"

# ------------------------------------------------------------------------------

res <- 150
png(paste0(figPath, "figure_axes_left_away.png"), width = 7, height = 2/3*7,
    units = "in", res = res)
phasePortrait("tan(z^3 + 1/2 - 2i)/(1 - 1i - z)",
              xlim = c(-6, 6), ylim = c(-3, 3),
              axes = FALSE, res = res)
dev.off()
# ------------------------------------------------------------------------------

res <- 150
png(paste0(figPath, "figure_with_box.png"), width = 7, height = 2/3*7,
    units = "in", res = res)
phasePortrait("tan(z^3 + 1/2 - 2i)/(1 - 1i - z)",
              xlim = c(-6, 6), ylim = c(-3, 3),
              axes = FALSE, res = res)
box()
dev.off()
# ------------------------------------------------------------------------------

res <- 150
png(paste0(figPath, "figure_black_bg_white_text.png"), width = 7, height = 2/3*7,
    units = "in", res = res)
op <- par(bg = "black", fg = "white")
# Setting the parameter fg has an effect on the box, the axes, and the axes'
# ticks, but not on the axis annotations and axis labels.
# Also the color of the title (main) is not affected.
# The colors of these elements have to be set manually and separately. While we
# could simply set them to "white", we set them, more flexibly, to the
# current foreground color (par("fg")).
phasePortrait("tan(z^3 + 1/2 - 2i)/(2 - 1i - z)",
              xlim = c(-6, 6), ylim = c(-3, 3),            col.axis = par("fg"),
              xlab = "real", ylab = "imaginary",           col.lab  = par("fg"),
              main = "All annotation in foreground color", col.main = par("fg"),
              # Adjust text size
              cex.axis = 0.9, cex.lab = 0.9)
par(op)
dev.off()
# ------------------------------------------------------------------------------

res <- 150
png(paste0(figPath, "figure_full_cover_device.png"),
    width = 7, height = 9/16*7,
    units = "in", res = res)
op <- par(mar = c(0, 0, 0, 0)) # Set plot margins to zero
xlim <- c(-3, 3)
# Calculate ylim with desired center fitting the desired aspect ratio
ylim <- ylimFromXlim(xlim, centerY = 0, x_to_y = 16/9)
phasePortrait(jacobiTheta, moreArgs = list(tau = 1i/5 + 1/5), pType = "p",
              xlim = xlim, ylim = ylim,
              xaxs = "i", yaxs = "i",
              axes = FALSE, res = 150)
par(op)
dev.off()
# ------------------------------------------------------------------------------

res <- 150
png(paste0(figPath, "figure_equal_margins.png"),
    width = 7, height = 9/16*7,
    units = "in", res = res)
# Set plot margins to zero, outer margins to 1/7 inch,
#   and background color to black
outerMar <- 1/7 # outer margin width in inches
op <- par(mar = c(0, 0, 0, 0), omi = rep(outerMar, 4), bg = "black")
xlim <- c(-1.5, 0.5)
# Calculate ylim with desired center fitting the desired aspect ratio;
#   however, the omi settings slightly change the required
#   ratio of xlim and ylim
ratio <- (7 - 2*outerMar) / (7 * 9/16 - 2*outerMar)
ylim  <- ylimFromXlim(xlim, centerY = 0, x_to_y = ratio)
phasePortrait("sin(jacobiTheta(z, tau))/z", moreArgs = list(tau = 1i/5 + 1/5),
              pType = "p",
              xlim = xlim, ylim = ylim,
              xaxs = "i", yaxs = "i",
              axes = FALSE, res = res)
par(op)
dev.off()
# ------------------------------------------------------------------------------
