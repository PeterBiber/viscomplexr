
library(viscomplexr)

# Ubuntu
setwd("/media/peter/Rindskanoppel/Peter/Projekte/viscomplexr/Peter")
tempDir <- "/media/peter/Rindskanoppel/temp/VisComplexFiles"

# windows
setwd("E:/Peter/Projekte/viscomplexr/Peter")
tempDir <- "E:/temp/VisComplexFiles"


# ------------------------------------------------------------------------------
# Chapman-Richards

curve(50*(1-exp(-0.04*x))^3, 0, 150)

x11(width = 29.7/2.54, height = 29.7/sqrt(2)/2.54); res <- 150
# x11(width = 29.7/2.54, height = 29.7/sqrt(2)/2.54); res <- 150
  # res <- 300; png("ChapmanRichards.png", 29.7, 29.7/sqrt(2), units = "in", res = res)
  op <- par(mar = c(0, 0, 0, 0), bg = "black", omi = c(1/4, 1/4, 1/4, 1/4))
  phasePortrait("50*(1-exp(-0.04*z))^3", xlim = c(-250, 250), ylim = c(-250, 250)/sqrt(2),
                res = res, axes = FALSE, xaxs = "i", yaxs = "i", pi2Div = 18,
                tempDir = tempDir, darkestShade = 0, pType = "pma")
  par(op)
  dev.off()

# ------------------------------------------------------------------------------
# Jetzt mit argOffset
phasePortrait("z", 4*c(-pi, pi), 4*c(-pi, pi), pType = "pma", tempDir = tempDir, argOffset = pi, pi2Div = 9)

# ------------------------------------------------------------------------------

x11(width = 16/9*8, height = 8); res <- 150
op <- par(mar = c(0, 0, 0, 0), omi = c(0.2, 0.2, 0.2, 0.2), bg = "black")
phasePortrait("z + 1/z", xlim = 16/9*c(-2, 2), ylim = c(-2, 2),
              res = res, pi2Div = 12, axes = FALSE, xaxs = "i", yaxs = "i",
              tempDir = tempDir, pType = "pma")
par(op)


x11(width = 16/9*8, height = 8); res <- 150
op <- par(mar = c(0, 0, 0, 0), omi = c(0.2, 0.2, 0.2, 0.2), bg = "black")
phasePortrait("tan(z + 1/z)", xlim = 16/9*c(-2, 2), ylim = c(-2, 2),
                res = res, pi2Div = 12, axes = FALSE, xaxs = "i", yaxs = "i",
                tempDir = tempDir, pType = "pma")
par(op)


x11(width = 16/9*8, height = 8); res <- 150
op <- par(mar = c(0, 0, 0, 0), omi = c(0.2, 0.2, 0.2, 0.2), bg = "black")
phasePortrait("sin(z + 1/z)", xlim = 16/9*c(-2, 2), ylim = c(-2, 2),
              res = res, pi2Div = 12, axes = FALSE, xaxs = "i", yaxs = "i",
              tempDir = tempDir, pType = "pma")
par(op)

# ------------------------------------------------------------------------------

x11(width = 16/9*8, height = 8); res <- 150
op <- par(mar = c(0, 0, 0, 0), omi = c(0.2, 0.2, 0.2, 0.2), bg = "black")
phasePortrait("(z + 1/z)/(1i/2 * (z-1)^10)", xlim = 16/9*c(-2, 2), ylim = c(-2, 2),
              res = res, pi2Div = 12, axes = FALSE, xaxs = "i", yaxs = "i",
              tempDir = tempDir, pType = "pma")
par(op)

x11(width = 16/9*8, height = 8); res <- 150
# res <- 600; png("Kernfusion.png", 16/9*30, 30, units = "cm", res = res)
op <- par(mar = c(0, 0, 0, 0), omi = c(0.2, 0.2, 0.2, 0.2), bg = "black")
phasePortrait("cos((z + 1/z)/(1i/2 * (z-1)^10))", xlim = 16/9*c(-2, 2), ylim = c(-2, 2),
              res = res, pi2Div = 9, axes = FALSE, xaxs = "i", yaxs = "i",
              tempDir = tempDir, pType = "pma")
par(op)
dev.off()


x11(width = 16/9*8, height = 8); res <- 150
op <- par(mar = c(0, 0, 0, 0), omi = c(0.2, 0.2, 0.2, 0.2), bg = "black")
phasePortrait("tan((z + 1/z)/(1i/2 * (z-1)^10))", xlim = 16/9*c(-2, 2), ylim = c(-2, 2),
              res = res, pi2Div = 9, axes = FALSE, xaxs = "i", yaxs = "i",
              tempDir = tempDir, pType = "pma")
par(op)


x11(width = 16/9*8, height = 8); res <- 150
# res <- 300; png("Lollipop.png", 16/9*30, 30, units = "cm", res = res)
op <- par(mar = c(0, 0, 0, 0), omi = c(0.2, 0.2, 0.2, 0.2), bg = "black")
phasePortrait("atan((z + 1/z)/(1i/2 * (z-1)^10))", xlim = 16/9*c(-2, 2), ylim = c(-2, 2),
              res = res, pi2Div = 9, axes = FALSE, xaxs = "i", yaxs = "i",
              tempDir = tempDir, pType = "pma")
par(op)
dev.off()

# ------------------------------------------------------------------------------
x11(width = 10, height = 10)
phasePortrait("(1-2*z^4)/z^6", xlim = c(-2, 2), ylim = c(-2, 2),
              main = parse(text = "f(z) == frac(1 - 2*z^4, z^6)"),
              tempDir = tempDir)


x11(width = 2.2/2*10, height = 10); res <- 300
  # png("Arcsin von gebrRational.png", 1.1 * 25, 25, units = "cm", res = res)
phasePortrait("acos((1-2*z^4)/z^6)", xlim = c(-2.2, 2.2), ylim = c(-2, 2),
              main = parse(text = "f(z) == arccos(frac(1 - 2*z^4, z^6))"),
              xlab = "real", ylab = "imaginary", res = res,
              tempDir = tempDir)
dev.off()


x11(width = 2.2/2*10, height = 10)
phasePortrait("asin((1-2*z^4)/z^6)", xlim = c(-2.2, 2.2), ylim = c(-2, 2),
              main = parse(text = "f(z) == arcsin(frac(1 - 2*z^4, z^6))"),
              xlab = "real", ylab = "imaginary",
              tempDir = tempDir)







# ------------------------------------------------------------------------------


