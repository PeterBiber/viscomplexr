

library(viscomplexr)

# setwd("E:/Peter/Projekte/viscomplexr/Peter")
# tempDir <- "E:/temp/VisComplexFiles"

setwd("/media/peter/Rindskanoppel/Peter/Projekte/viscomplexr/Peter")
tempDir <- "/media/peter/Rindskanoppel/temp/VisComplexFiles"


complexFunctionPlot("z", xlim = c(-20, 20), ylim = c(-20, 20), tempDir = tempDir)

complexFunctionPlot("exp(z)", xlim = c(-10, 10), ylim = c(-10, 10), tempDir = tempDir)

# -----------------------------------------------------------------------------
# Butterworth-Filter - schönes Bild, aber reproduziert noch nicht das Okinal?!
# Ganz kommt es nicht hin, obwohl ich keinen Fehler in der Gleichung sehe ...

windows(10, 10)
phasePortrait("vapply(z, function(z, n, omegaNull) {
                        w <- 1
                        for(k in c(1:n)) {
                          zk <- 1i*exp(1i*(2*k - 1)/(2*n))
                          w <- w / (z/omegaNull - zk)
                        }
                        return(w)
                    }, n = 10, omegaNull = 1, FUN.VALUE = complex(1))",
                    xlim = 1.5*c(-1, 1), ylim = 1.5*c(-1, 1), tempDir = tempDir, pType = "pm")
abline(h = 0, col = "grey", lty = "dashed")
abline(v = 0, col = "grey", lty = "dashed")

# -----------------------------------------------------------------------------
# S. 27 - auch das kommt nicht ganz hin.
# Richtig ist f(z) = ((1i*z)^(-17) - 1)/(1i*z - 1)*exp(1i*pi)

# windows(8, 8)
x11(width = 8, height = 8)
phasePortrait("((1i*z)^(-17) - 1)/(1i*z - 1)*exp(1i*pi)",
              xlim = 1.2*c(-1, 1), ylim = 1.2*c(-1, 1), tempDir = tempDir, pType = "p")

# windows(8, 8)
x11(width = 8, height = 8)
phasePortrait("((1i*z)^(-17) - 1)",
              xlim = 1.2*c(-1, 1), ylim = 1.2*c(-1, 1), tempDir = tempDir, pType = "pma")

x11(width = 8, height = 8)
phasePortrait("(1i*z - 1)",
              xlim = 1.2*c(-1, 1), ylim = 1.2*c(-1, 1), tempDir = tempDir, pType = "pma")


# Drei nebeneinander, p, pm und pma
x11(width = 16, height = 9); res <- 150
# res <- 600; png("Wegert Rosette 17Wurzel Ubu.png", 16, 9, units = "in", res = res)
op <- par(mfrow = c(1, 3), mar = c(0, 0, 0, 0), bg = "black")
phasePortrait("((1i*z)^(-17) - 1)/(1i*z - 1)*exp(1i*pi)",
              xlim = 1.2*c(-1, 1), ylim = 1.2*c(-1, 1), tempDir = tempDir, pType = "p",
              axes = FALSE, xaxs = "i", yaxs = "i", res = res)
phasePortrait("((1i*z)^(-17) - 1)/(1i*z - 1)*exp(1i*pi)",
              xlim = 1.2*c(-1, 1), ylim = 1.2*c(-1, 1), tempDir = tempDir, pType = "pm",
              axes = FALSE, xaxs = "i", yaxs = "i", res = res)
phasePortrait("((1i*z)^(-17) - 1)/(1i*z - 1)*exp(1i*pi)",
              xlim = 1.2*c(-1, 1), ylim = 1.2*c(-1, 1), tempDir = tempDir, pType = "pma",
              axes = FALSE, xaxs = "i", yaxs = "i", res = res)
par(op)
dev.off()

# -----------------------------------------------------------------------------
# S. 28 - Standardbeispiel

x11(width = 8, height = 8); res <- 150
# windows(width = 8, height = 8); res <- 150
phasePortrait("(z - 1)/(z^2 +  z + 1)", xlim = 1*2/5*c(-5, 5), ylim = 1*2/5*c(-5, 5), res = res,
                main = expression(f(z) == frac((z-1),(z^2+z+1))), tempDir = tempDir,
                pType = "pa", pi2Div = 1, argOffset = pi) # sehr schön mit pi2Div = 4

# -----------------------------------------------------------------------------
# S. 37 Sein rechtes Beispiel stimmt gar nicht, und im linken hat er -1 geschrieben,
# wo es +1 heißen muss. Bin nicht mehr besonders begeistert. Habe nach vielem
# Herumspielen jetzt auch keine Lust mehr, die richtige Funktion zu erraten.´
# Nachtrag: Er hat bei beiden -1 geschrieben, wo es +1 heißen muss.
# Jetzt funktioniert es.

x11(width = 16, height = 8)
op <- par(mfrow = c(1, 2))
phasePortrait("(z - 1)/(z^2 + z + 1)",
              xlim = c(-2, 2), ylim = c(-2, 2), tempDir = tempDir, pi2Div = 24)
phasePortrait("(z - 1)*(Conj(z)^2 + Conj(z) + 1)",
              xlim = c(-2, 2), ylim = c(-2, 2), tempDir = tempDir, pi2Div = 24)
par(op)

# -----------------------------------------------------------------------------
# S. 41 Obere und untere Halbkugel der Riemann-Sphäre
x11(width = 16, height = 8)
op <- par(mfrow = c(1, 2))
phasePortrait("z", main = "Riemann Sphere - Lower Half",
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 30)
phasePortrait("1/z * exp(1i*pi)", main = "Riemann Sphere - Upper Half",
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 30)
par(op)


# Angewandt auf das Standardbeispiel
x11(width = 16, height = 8)
op <- par(mfrow = c(1, 2))
phasePortrait("(z - 1)/(z^2 + z + 1)",
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 30)
bigC <- list(x = 2 * cos(c(1:360)*2*pi/360), y = 2 * sin(c(1:360)*2*pi/360))
untC <- list(x = 1 * cos(c(1:360)*2*pi/360), y = 1 * sin(c(1:360)*2*pi/360))
polypath(x = c(untC$x, NA_real_, bigC$x), y = c(untC$y, NA_real_, bigC$y),
         col = adjustcolor( "white", alpha.f = 0.4),
         rule = "evenodd",
         lwd = 2)
# plotrix::draw.circle(0, 0, 1, lwd = 2)

phasePortrait("vapply(z, function(z) {
                 z <- 1/(z * exp(1i*pi))
                 w <- (z - 1)/(z^2 + z + 1)
                 return(w)
              }, FUN.VALUE = complex(1))",
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 30)
polypath(x = c(untC$x, NA_real_, bigC$x), y = c(untC$y, NA_real_, bigC$y),
         col = adjustcolor( "white", alpha.f = 0.4),
         rule = "evenodd",
         lwd = 2)
# plotrix::draw.circle(0, 0, 1, lwd = 2)
par(op)


# Zum Shaden von:
# https://r.789695.n4.nabble.com/fill-the-area-outside-a-polygon-td4715148.html
p1 <- list(x = c(20, 80, 80, 20), y= c(20, 20, 80, 80))
p2 <- list(x = c(0,100,100,0), y = c(0,0,100,100))
plot(p2, type="n")
polypath(x = c(p1$x, NA_real_, p2$x), y = c(p1$y, NA_real_, p2$y), col =
           "lightblue", rule = "evenodd")


# S. 63 - Polynome
x11(width = 16, height = 8)
op <- par(mfrow = c(1, 2))
phasePortrait("z^5 - z^4 - z + 1", pType = "pma",
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 30)
bigC <- list(x = c(-2, -2, 2, 2), y = c(-2, 2, 2, -2))
untC <- list(x = 1 * cos(c(1:360)*2*pi/360), y = 1 * sin(c(1:360)*2*pi/360))
polypath(x = c(untC$x, NA_real_, bigC$x), y = c(untC$y, NA_real_, bigC$y),
         col = adjustcolor( "white", alpha.f = 0.4),
         rule = "evenodd",
         lwd = 2)

phasePortrait("z^5 - z^4 - z + 1", pType = "pma", invertFlip = TRUE,
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 30)
polypath(x = c(untC$x, NA_real_, bigC$x), y = c(untC$y, NA_real_, bigC$y),
         col = adjustcolor( "white", alpha.f = 0.4),
         rule = "evenodd",
         lwd = 2)
par(op)

# -----------------------------------------------------------------------------


# Mit Bezug auf S. 63 - Was von Peter
x11(width = 16, height = 8)
op <- par(mfrow = c(1, 2))
phasePortrait("z^-3 - z^2", pType = "pma",
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 30)
bigC <- list(x = c(-2, -2, 2, 2), y = c(-2, 2, 2, -2))
untC <- list(x = 1 * cos(c(1:360)*2*pi/360), y = 1 * sin(c(1:360)*2*pi/360))
polypath(x = c(untC$x, NA_real_, bigC$x), y = c(untC$y, NA_real_, bigC$y),
         col = adjustcolor( "white", alpha.f = 0.4),
         rule = "evenodd",
         lwd = 2)

phasePortrait("z^-3 - z^2", pType = "pma", invertFlip = TRUE,
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 30)
polypath(x = c(untC$x, NA_real_, bigC$x), y = c(untC$y, NA_real_, bigC$y),
         col = adjustcolor( "white", alpha.f = 0.4),
         rule = "evenodd",
         lwd = 2)
par(op)

# -----------------------------------------------------------------------------

# Test invertFlip
x11(width = 16, height = 8)
# png("RSPH Phase Portrait 1.png", 16, 8, units = "in", res = 150)
op <- par(mfrow = c(1, 2))
phasePortrait("(z - 1)/(z^2 + z + 1)", pType = "pma",
              main = "Southern Riemann Hemisphere",
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 30,
              xlab = "real", ylab = "imaginary")
bigC <- list(x = c(-2, -2, 2, 2), y = c(-2, 2, 2, -2))
untC <- list(x = 1 * cos(c(1:360)*2*pi/360), y = 1 * sin(c(1:360)*2*pi/360))
polypath(x = c(untC$x, NA_real_, bigC$x), y = c(untC$y, NA_real_, bigC$y),
         col = adjustcolor( "white", alpha.f = 0.5),
         rule = "evenodd",
         lwd = 2)
text(0.8, 1, expression(f(z)==frac(z-1,z^2+z+1)))

phasePortrait("(z - 1)/(z^2 + z + 1)", pType = "pma",
              main = "Northern Riemann Hemisphere",
              invertFlip = TRUE,
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 30,
              xlab = "real", ylab = "imaginary")
polypath(x = c(untC$x, NA_real_, bigC$x), y = c(untC$y, NA_real_, bigC$y),
         col = adjustcolor( "white", alpha.f = 0.5),
         rule = "evenodd",
         lwd = 2)
text(0.8, 1, expression(f(z)==frac(z-1,z^2+z+1)))
par(op)
dev.off()

# -----------------------------------------------------------------------------

# Test invertFlip
x11(width = 16, height = 8)
# png("Exp Riemann Phase Portrait 1.png", 16, 8, units = "in", res = 150)
op <- par(mfrow = c(1, 2))
phasePortrait("exp(z)", pType = "pma",
              main = "Southern Riemann Hemisphere",
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 30,
              xlab = "real", ylab = "imaginary")
bigC <- list(x = c(-2, -2, 2, 2), y = c(-2, 2, 2, -2))
untC <- list(x = 1 * cos(c(1:360)*2*pi/360), y = 1 * sin(c(1:360)*2*pi/360))
polypath(x = c(untC$x, NA_real_, bigC$x), y = c(untC$y, NA_real_, bigC$y),
         col = adjustcolor( "white", alpha.f = 0.5),
         rule = "evenodd",
         lwd = 2)
text(0.8, 1, expression(f(z)==e^z))

phasePortrait("exp(z)", pType = "pma",
              main = "Northern Riemann Hemisphere",
              invertFlip = TRUE,
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 30,
              xlab = "real", ylab = "imaginary")
polypath(x = c(untC$x, NA_real_, bigC$x), y = c(untC$y, NA_real_, bigC$y),
         col = adjustcolor( "white", alpha.f = 0.5),
         rule = "evenodd",
         lwd = 2)
text(0.8, 1, expression(f(z)==e^z))
par(op)
dev.off()

# -----------------------------------------------------------------------------

# Test riemannMask and invertFlip
x11(width = 16, height = 8)
# png("Sin Riemann Phase Portrait 1.png", 16, 8, units = "in", res = 150)
op <- par(mfrow = c(1, 2))
phasePortrait("sin(z)", pType = "pma",
              main = "Southern Riemann Hemisphere",
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 30,
              xlab = "real", ylab = "imaginary")
riemannMask()
text(0.8, 1, expression(f(z)==sin(z)))

phasePortrait("sin(z)", pType = "pma",
              main = "Northern Riemann Hemisphere",
              invertFlip = TRUE,
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 30,
              xlab = "real", ylab = "imaginary")
riemannMask()
text(0.8, 1, expression(f(z)==sin(z)))
par(op)
dev.off()

# -----------------------------------------------------------------------------

# Test riemannMask and invertFlip
x11(width = 16, height = 8)
# png("Cos Riemann Phase Portrait 1.png", 16, 8, units = "in", res = 150)
op <- par(mfrow = c(1, 2))
phasePortrait("cos(z)", pType = "pma",
              main = "Southern Riemann Hemisphere",
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 30,
              xlab = "real", ylab = "imaginary")
riemannMask()
text(0.8, 1, expression(f(z)==cos(z)))

phasePortrait("cos(z)", pType = "pma",
              main = "Northern Riemann Hemisphere",
              invertFlip = TRUE,
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 30,
              xlab = "real", ylab = "imaginary")
riemannMask()
text(0.8, 1, expression(f(z)==cos(z)))
par(op)
dev.off()

# -----------------------------------------------------------------------------

# Test riemannMask and invertFlip
x11(width = 16.1, height = 8.1)
# png("Tan Riemann Phase Portrait 1.png", 16, 8, units = "in", res = 150)
op <- par(mfrow = c(1, 2),mar = c(4.5, 4.5, 3.5, 3.5))
phasePortrait("tan(z)", pType = "pma",
              main = "Southern Riemann Hemisphere",
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 30,
              xlab = "real", ylab = "imaginary", xaxs = "i", yaxs = "i")
riemannMask(annotSouth = TRUE, gridCross = TRUE)
text(0.8, 1, expression(f(z)==tan(z)))

phasePortrait("tan(z)", pType = "pma",
              main = "Northern Riemann Hemisphere",
              invertFlip = TRUE,
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 30,
              xlab = "real", ylab = "imaginary", xaxs = "i", yaxs = "i")
riemannMask(annotNorth = TRUE, gridCross = TRUE)
text(0.8, 1, expression(f(z)==tan(z)))
par(op)
dev.off()

# -----------------------------------------------------------------------------

x11(width = 16, height = 8); res <- 300
# windows(width = 16, height = 8)
# res <- 600; png("Z Riemann Phase Portrait 1.png", 16, 8, units = "in", res = res)
op <- par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3.5, 3.5))
phasePortrait("z", pType = "pma",
              main = "Southern Riemann Hemisphere",
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 9,
              xlab = "real", ylab = "imaginary", res = res)
riemannMask(annotSouth = TRUE, gridCross = TRUE, circLwd = 1)

phasePortrait("z", pType = "pma",
              main = "Northern Riemann Hemisphere",
              invertFlip = TRUE,
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 9,
              xlab = "real", ylab = "imaginary", res = res)
riemannMask(annotNorth = TRUE, gridCross = TRUE, circLwd = 1)
par(op)
dev.off()

# -----------------------------------------------------------------------------

x11(width = 16, height = 8)
# png("Logistic Riemann Phase Portrait 1.png", 16, 8, units = "in", res = 150)
op <- par(mfrow = c(1, 2), mar = c(4.7, 4.7, 3.5, 3.5))
phasePortrait("cos(1/(1 + exp(-5*z)))", pType = "pma",
              main = "Southern Riemann Hemisphere",
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 9,
              xlab = "real", ylab = "imaginary", xaxs = "i", yaxs = "i")
riemannMask(annotSouth = TRUE, gridCross = TRUE)

phasePortrait("cos(1/(1 + exp(-5*z)))", pType = "pma",
              main = "Northern Riemann Hemisphere",
              invertFlip = TRUE,
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), tempDir = tempDir, pi2Div = 9,
              xlab = "real", ylab = "imaginary", xaxs = "i", yaxs = "i")
riemannMask(annotNorth = TRUE, gridCross = TRUE)
par(op)
dev.off()

# -----------------------------------------------------------------------------
