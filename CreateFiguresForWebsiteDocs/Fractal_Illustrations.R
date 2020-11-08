# ------------------------------------------------------------------------------
# Creating illustrations for the documentation on the website

# Fractals section


figPath <- "CreateFiguresForWebsiteDocs/"

# ------------------------------------------------------------------------------
res  <- 300 # set resolution
# open png graphics device with in DIN A4 format
# DIN A format has an edge length ratio of sqrt(2)
png(paste0(figPath, "figure_Mandelbrot_Example.png"),
    width = 14, height = 14/sqrt(2), # DIN A4 like side ratio
    units = "in",
    res = res)                   # resolution is required
op   <- par(mar = c(0, 0, 0, 0)) # set graphics parameters - no plot margins
xlim <- c(-1.254, -1.248)        # horizontal (real) plot limits
# the function below adjusts the imaginary plot limits to the
#   desired ratio (sqrt(2)) centered around the desired imaginary value
ylim <- ylimFromXlim(xlim, centerY = 0.02, x_to_y = sqrt(2))
phasePortrait(mandelbrot,
              nCores = 1,             # Increase or leave out for higher performance
              xlim = xlim, ylim = ylim,
              hsvNaN = c(0, 0, 0),    # Black color for NaN results
              xaxs = "i", yaxs = "i", # suppress R's default axis margins
              axes = FALSE,           # do not plot axes
              res = res)              # resolution is required
par(op)   # reset graphics parameters
dev.off() # close graphics device and complete the png file
# ------------------------------------------------------------------------------

res <- 300
png(paste0(figPath, "figure_Julia_Example_1.png"), width = 14, height = 14/sqrt(2),
    units = "in", res = res)
op <- par(mar = c(0, 0, 0, 0))
xlim <- c(-1.8, 1.8)
ylim <- ylimFromXlim(xlim, centerY = 0, x_to_y = sqrt(2))
phasePortrait(juliaNormal,
              # see documentation of juliaNormal about the arguments
              #  c and R_esc
              moreArgs = list(c = -0.09 - 0.649i, R_esc = 2),
              nCores = 1, # Increase or leave out for higher performance
              xlim = xlim, ylim = ylim,
              hsvNaN = c(0, 0, 0),
              xaxs = "i", yaxs = "i",
              axes = FALSE,
              res = res)
par(op)
dev.off()
# ------------------------------------------------------------------------------

res <- 300
png(paste0(figPath, "figure_Julia_Example_2.png"), width = 7, height = 7/sqrt(2),
    units = "in", res = res)
op <- par(mar = c(0, 0, 0, 0))
xlim <- c(-0.32, 0.02)
ylim <- ylimFromXlim(xlim, center = -0.78, x_to_y = sqrt(2))
phasePortrait(juliaNormal,
              # see documentation of juliaNormal about the arguments
              #  c and R_esc
              moreArgs = list(c = -0.119 - 0.882i, R_esc = 2),
              nCores = 1, # Increase or leave out for higher performance
              xlim = xlim, ylim = ylim,
              hsvNaN = c(0, 0, 0),
              xaxs = "i", yaxs = "i",
              axes = FALSE,
              res = res)
par(op)
dev.off()
# ------------------------------------------------------------------------------

