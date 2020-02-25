# -----------------------------------------------------------------------------
library(viscomplexr)

# setwd("E:/Peter/Projekte/viscomplexr/Peter")
# tempDir <- "E:/temp/VisComplexFiles"

setwd("/media/peter/Rindskanoppel/Peter/Projekte/viscomplexr/Peter")
tempDir <- "/media/peter/Rindskanoppel/temp/VisComplexFiles"

# -----------------------------------------------------------------------------

x11(width = 16, height = 8)
# png("StrangeFlower Riemann Phase Portrait 1.png", 16, 8, units = "in", res = 150)
op <- par(mfrow = c(1, 2), mar = c(4.7, 4.7, 3.5, 3.5))
phasePortrait("(-z^17 - z^15 - z^9 - z^7 - z^2 - z + 1)/(1i*z - 1)", pType = "pma",
              main = "Southern Riemann Hemisphere",
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 9,
              xlab = "real", ylab = "imaginary", xaxs = "i", yaxs = "i")
riemannMask(annotSouth = FALSE, gridCross = FALSE)

phasePortrait("(-z^17 - z^15 - z^9 - z^7 - z^2 - z + 1)/(1i*z - 1)", pType = "pma",
              main = "Northern Riemann Hemisphere",
              invertFlip = TRUE,
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 9,
              xlab = "real", ylab = "imaginary", xaxs = "i", yaxs = "i")
riemannMask(annotNorth = FALSE, gridCross = FALSE)
par(op)
dev.off()

# -----------------------------------------------------------------------------

# https://de.wikibooks.org/wiki/Mathe_f%C3%BCr_Nicht-Freaks:_Komplexe_Zahlen:_Darstellung_komplexwertiger_Funktionen

x11(width = 16, height = 8)
# png("StrangeFlower2 Riemann Phase Portrait 1.png", 16, 8, units = "in", res = 150)
op <- par(mfrow = c(1, 2), mar = c(4.7, 4.7, 3.5, 3.5))
fFun <- "1/(z^7 - z)"
phasePortrait(fFun, pType = "pma",
              main = "Southern Riemann Hemisphere",
              xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), tempDir = tempDir, pi2Div = 9,
              xlab = "real", ylab = "imaginary", xaxs = "i", yaxs = "i")
# riemannMask(annotSouth = FALSE, gridCross = FALSE)

phasePortrait(fFun, pType = "pma",
              main = "Northern Riemann Hemisphere",
              invertFlip = TRUE,
              xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), tempDir = tempDir, pi2Div = 9,
              xlab = "real", ylab = "imaginary", xaxs = "i", yaxs = "i")
# riemannMask(annotNorth = FALSE, gridCross = FALSE)
par(op)
dev.off()

# -----------------------------------------------------------------------------

x11(width = 16, height = 8)
# windows(16, 8)
res <- 150
  # res <- 600; png("StrangeFlower3 Riemann Phase Portrait 1.png", 28, 14, units = "cm", res = res)
  op <- par(mfrow = c(1, 2), mar = c(2, 2, 2, 2), bg = "black")
  fFun <- "(z^5 + 1i*z - 1)/(z^11 - z)"
  phasePortrait(fFun, pType = "pma",
                main = "Southern Riemann Hemisphere",
                xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), tempDir = tempDir, pi2Div = 9,
                xlab = "real", ylab = "imaginary", xaxs = "i", yaxs = "i", res = res)
  riemannMask(annotSouth = FALSE, gridCross = FALSE,
              circOutline = FALSE, alphaMask = 0.2)

  phasePortrait(fFun, pType = "pma",
                main = "Northern Riemann Hemisphere",
                invertFlip = TRUE,
                xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), tempDir = tempDir, pi2Div = 9,
                xlab = "real", ylab = "imaginary", xaxs = "i", yaxs = "i", res = res)
  riemannMask(annotNorth = FALSE, gridCross = FALSE,
              circOutline = FALSE, alphaMask = 0.2)
  par(op)
  dev.off()

# -----------------------------------------------------------------------------
# Die Riemann'sche Zeta-Funktion

library(viscomplexr)
library(pracma)

tempDir <- "/media/peter/Rindskanoppel/temp/VisComplexFiles"

x11(width = sqrt(2)*8, height = 8); res <- 150
# res <- 300; png("pracmaz.png", width = sqrt(2)*8,
#                 height = 8, units = "in", res = res)
phasePortrait("zeta(z)", xlim = c(-35, 35), res = res,
              ylim = c(-20, 20), tempDir = tempDir)
dev.off()


# Jetzt noch einen Riemann-Spären-Plot
x11(width = 16, height = 8); res <- 150
op <- par(mfrow = c(1, 2))
phasePortrait("zeta(z)", xlim = c(-1.3, 1.3), res = res,
              ylim = c(-1.3, 1.3), tempDir = tempDir)
riemannMask(annotSouth = FALSE, gridCross = FALSE,
            circOutline = FALSE)
phasePortrait("zeta(z)", xlim = c(-1.3, 1.3), res = res,
              ylim = c(-1.3, 1.3), invertFlip = TRUE,
              tempDir = tempDir)
riemannMask(annotNorth = FALSE, gridCross = FALSE,
            circOutline = FALSE)
par(op)

# -----------------------------------------------------------------------------




