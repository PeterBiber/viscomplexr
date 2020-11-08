# ------------------------------------------------------------------------------
# Creating illustrations for the documentation on the website

# Aesthetics section


figPath <- "CreateFiguresForWebsiteDocs/"

# ------------------------------------------------------------------------------
# Make the banner for the index.html file in 300 dpi resolution
res <- 300
png(paste0(figPath, "figure_banner_300dpi.png"), width = 9, height = 0.4*9,
    units = "in", res = res)
# reduce plot margin to 0, store previous graphics parameter settings in op
op <- par(mar = c(0, 0, 0, 0))
# call the function phasePortrait
phasePortrait("sin(z^3/(z - 1i + 2))",                # define the function
              xlim = c(-6, 6), ylim = c(-2.4, 2.4),   # define the domain
              axes = FALSE,                           # suppress axes
              xaxs = "i", yaxs = "i",                 # no empty zone around plot
              res = res)
# set graphics parameters to previous values
par(op)
dev.off()
# ------------------------------------------------------------------------------
