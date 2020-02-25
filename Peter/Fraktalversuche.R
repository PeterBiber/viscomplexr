# ------------------------------------------------------------------------------------------------

# Versuche zur Optimierung des Apfelmännchen in viscomplex

# ------------------------------------------------------------------------------------------------
# Apfelmännchen-Gleichung

# Versuch zu optimieren

# Breite DINA4, Höhe aber 9/16
windows(29.7/2.54, 9/16*29.7/2.54); res <- 100
# res <- 600; png("Apfelmaennchen optim anders herum 1 16 zu 9 600 dpi n = 300.png", 29.7, 9/16*29.7, units = "cm", res = res)
nCores <- 8
op <- par(mar = c(0, 0, 0, 0), bg = "black")
ylim       <- c(-1.2, 1.2)
yRange     <- abs(ylim[2] - ylim[1])
xlimOrig   <- c(-2, 1)
xRangeOrig <- abs(xlimOrig[2] - xlimOrig[1])
xyfac      <- xRangeOrig / yRange
xRange   <- abs(xlimOrig[2] - xlimOrig[1]) * 16/9 / xyfac
xlim     <- c(mean(xlimOrig) - xRange/2, mean(xlimOrig) + xRange/2)
system.time(
complexFunctionPlot(exprText = "vapply(z, FUN = ff <- function(z, nMax) {
                        c  <- z
                        zz <- 0
                        for(i in c(1:nMax)) {
                          zz    <- zz^2 + c
                          if(Mod(zz) > 4) {
                            zz <- NaN
                            break()
                          }
                        }  
                        return(zz)
                    }, nMax = 300,
                    FUN.VALUE = complex(1))",
                    xlim = xlim, ylim = ylim,
                    main = "z", axes = TRUE, pType = "pma",
                    xlab = "real", ylab = "imaginary", logBase = 1.5, nCores = nCores,
                    tempDir = tempDir, res = res, hsvNaN = c(0, 0, 0), xaxs = "i", yaxs = "i")
) # system.time
text(1.3, -1.17, "R package viscomplexr by P. Biber", col = "darkgrey", cex = 0.7)
par(op)
dev.off()



