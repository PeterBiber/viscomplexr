


# setwd("E:/Peter/Projekte/viscomplexr/Peter")
setwd("/media/peter/Rindskanoppel/Peter/Projekte/viscomplexr/Peter")


# tempDir <- getwd()
# tempDir <- "E:/temp/VisComplexFiles"
tempDir <- "/media/peter/Rindskanoppel/temp/VisComplexFiles"

# windows(10, 10); res <- 220
x11(width = 10, height = 10); res <- 220
nCores <- 8
# system.time(
complexFunctionPlot(exprText = "z", xlim = c(-8, 8), ylim = c(-8, 8),
                    main = "z", axes = TRUE, pType = "pma",
                    xlab = "real", ylab = "imaginary", logBase = 2, nCores = nCores,
                    tempDir = tempDir, res = res)
# ) # system.time
foreach::registerDoSEQ()


# Von Johnny #1

# windows(16, 9); res <- 150
x11(width = 16, height = 9); res <- 150
nCores <- 8
# res = 600; png("Höhö 600dpi.png", 16, 9, units = "in", res = res)
op <- par(mar = c(0, 0, 0, 0), bg = "black")
complexFunctionPlot(exprText = "sin(1/sin(z*0.75))*tan(1i*z)", xlim = c(-7.5, 7.5), ylim = c(-4.22, 4.22),
                    main = "", # parse(text = "f(z) == (z^2 - 1i)*sin(z - 1)/(-1i+2+z)*exp(-1i*pi)"),
                    axes = FALSE, pType = "pma", logBase = 2, res = res,
                    xlab = "real", ylab = "imaginary",
                    darkestShade = 0,
                    xaxs = "i", yaxs = "i", nCores = nCores)
par(op)
dev.off()


# Von Johnny #2

# windows(16, 9); res <- 300
x11(width = 16, height = 9); res <- 300
# res = 600; png("Ultra Tangens 1000dpi.png", 16, 9, units = "in", res = res)
op <- par(mar = c(0, 0, 0, 0), bg = "black")
complexFunctionPlot(exprText = "sin(tan(exp(z/2*exp(1i*pi/6))))*exp(1i*pi/4)", xlim = c(-7.5, 7.5), ylim = c(-4.22, 4.22),
                    main = "", # parse(text = "f(z) == (z^2 - 1i)*sin(z - 1)/(-1i+2+z)*exp(-1i*pi)"),
                    axes = FALSE, pType = "pma", logBase = 2, res = res,
                    xlab = "real", ylab = "imaginary",
                    darkestShade = 0,
                    xaxs = "i", yaxs = "i")
par(op)
dev.off()


# Von Johnny #3

# windows(16, 9); res <- 150
x11(width = 16, height = 9); res <- 150
# res = 600; png("Ultra Tangens 600dpi Ubu.png", 16, 9, units = "in", res = res)
op <- par(mar = c(0, 0, 0, 0), bg = "black")
complexFunctionPlot(exprText = "tan((z^2 - 1i)*sin(z - 1)/(-1i+2+z)*exp(-1i*pi))", xlim = 2.5*c(-4, 2), ylim = c(-4.22, 4.22),
                    main = "", # parse(text = "f(z) == (z^2 - 1i)*sin(z - 1)/(-1i+2+z)*exp(-1i*pi)"),
                    axes = FALSE, pType = "pma", logBase = 2, res = res,
                    xlab = "real", ylab = "imaginary",
                    darkestShade = 0,
                    xaxs = "i", yaxs = "i")
par(op)
dev.off()



windows(16, 9); res <- 600
# res = 600; png("Ultra Tangens 600dpi.png", 16, 9, units = "in", res = res)
op <- par(mar = c(0, 0, 0, 0), bg = "black")
complexFunctionPlot(exprText = "sin(tan(z/2*exp(1i*pi/6)))*exp(1i*pi/4)", xlim = c(-7.5, 7.5), ylim = c(-4.22, 4.22),
                    main = "", # parse(text = "f(z) == (z^2 - 1i)*sin(z - 1)/(-1i+2+z)*exp(-1i*pi)"),
                    axes = FALSE, pType = "pma", logBase = 2, res = res,
                    xlab = "real", ylab = "imaginary",
                    darkestShade = 0,
                    xaxs = "i", yaxs = "i")
par(op)
dev.off()


# Anschauliche Analysis 2, S. 143 unten
# Schaut super aus!
windows(16, 9); res <- 150
# res = 600; png("XXXTest.png", 16, 9, units = "in", res = res)
op <- par(mar = c(0, 0, 0, 0), bg = "black")
complexFunctionPlot(exprText = "1/4*z^2 - 10*z/(z^4+4)", xlim = c(-7.5, 7.5), ylim = c(-4.22, 4.22),
                    main = "",
                    axes = FALSE, pType = "pma", logBase = 2, res = res,
                    xlab = "real", ylab = "imaginary",
                    darkestShade = 0,
                    xaxs = "i", yaxs = "i", tempDir = tempDir)
par(op)
dev.off()


windows(16, 9); res <- 150
# res = 600; png("XXXTest.png", 16, 9, units = "in", res = res)
op <- par(mar = c(0, 0, 0, 0), bg = "black")
complexFunctionPlot(exprText = "tan(1/4*z^2 - 10*z/(z^4+4))", xlim = c(-7.5, 7.5), ylim = c(-4.22, 4.22),
                    main = "",
                    axes = FALSE, pType = "pma", logBase = 2, res = res,
                    xlab = "real", ylab = "imaginary",
                    darkestShade = 0,
                    xaxs = "i", yaxs = "i", tempDir = tempDir)
par(op)
dev.off()


# windows(16, 9); res <- 150
x11(width = 16, height = 9); res <- 150
# res = 600; png("Krasser Sinus I Ubu.png", 16, 9, units = "in", res = res)
op <- par(mar = c(0, 0, 0, 0), bg = "black")
system.time(
complexFunctionPlot(exprText = "sin(1/4*z^2 - 10*z/(z^4+4))", xlim = c(-7.5, 7.5), ylim = c(-4.22, 4.22),
                    main = "",
                    axes = FALSE, pType = "pma", logBase = 2, res = res,
                    xlab = "real", ylab = "imaginary",
                    darkestShade = 0,
                    xaxs = "i", yaxs = "i", tempDir = tempDir)
) # system.time
par(op)
dev.off()


# "(z^2 - 1i)/(tan(z))"
# windows(16, 9); res <- 150
x11(width = 16, height = 9); res <- 150
  # res = 600; png("Parabel geteilt durch Tangens Ubu.png", 16, 9, units = "in", res = res)
  op <- par(mar = c(0, 0, 0, 0), bg = "black")
  complexFunctionPlot(exprText = "(z^2 - 1i)/(tan(z))", xlim = c(-7.5, 7.5), ylim = c(-4.22, 4.22),
                      main = "",
                      axes = FALSE, pType = "pma", logBase = 2, res = res,
                      xlab = "real", ylab = "imaginary",
                      darkestShade = 0,
                      xaxs = "i", yaxs = "i", tempDir = tempDir)
  par(op)
  dev.off()


# windows(16, 9); res <- 150
x11(width = 16, height = 9); res <- 150
# res = 600; png("Parabel geteilt durch PolySinus Ubu.png", 16, 9, units = "in", res = res)
op <- par(mar = c(0, 0, 0, 0), bg = "black")
complexFunctionPlot(exprText = "(z^2 - 1i)/(sin(z/3 - 1i*(z/2)^2))", xlim = c(-7.5, 7.5), ylim = c(-4.22, 4.22),
                    main = "",
                    axes = FALSE, pType = "pma", logBase = 2, res = res,
                    xlab = "real", ylab = "imaginary",
                    darkestShade = 0,
                    xaxs = "i", yaxs = "i", tempDir = tempDir)
par(op)
dev.off()



# Anschauliche Analysis S. 163, versuch, eine Funktion zu übergeben
# windows(16, 9); res <- 150
x11(width = 16, height = 9); res <- 150
# res = 600; png("AXXX Krasser Sinus I.png", 16, 9, units = "in", res = res)
op <- par(mar = c(0, 0, 0, 0), bg = "black")
complexFunctionPlot(exprText = "vapply(z, FUN = ff <- function(z, a, phi)
                                       {return(  ((a*z*exp(1i*phi))^2 - 1) / (z*exp(1i*phi))^2-a) },
                                       a = 1.5, phi = pi/4,
                                       FUN.VALUE = complex(1))",
                    xlim = c(-7.5, 7.5), ylim = c(-4.22, 4.22),
                    main = "",
                    axes = FALSE, pType = "pma", logBase = 2, res = res,
                    xlab = "real", ylab = "imaginary",
                    darkestShade = 0,
                    xaxs = "i", yaxs = "i", tempDir = tempDir)
par(op)
dev.off()



# Weiterer Test: Funktion übergeben

# Irre 1!
# windows(10, 10); res <- 150
x11(width = 16, height = 9); res <- 150
nCores <- 8
limFac <- 0.8
phasePortrait(exprText = "vapply(z, FUN = ff <- function(z, n, c, exP) {
                        for(i in c(1:n)) {
                          zNew <- z^exP + c
                          z    <- zNew
                        }
                        return(zNew)
                    }, n = 7, c = 1, exP = 3,
                    FUN.VALUE = complex(1))",
                    xlim = limFac*c(-2, 2), ylim = limFac*c(-2, 2),
                    main = "z", axes = TRUE, pType = "pm",
                    xlab = "real", ylab = "imaginary", logBase = 2, nCores = nCores,
                    tempDir = tempDir, res = res, hsvNaN = c(0, 0, 0))


# Irre 1!
windows(10, 10); res <- 100
nCores <- 8
limFac <- 0.8
complexFunctionPlot(exprText = "vapply(z, FUN = ff <- function(z, n, c, exP) {
                        for(i in c(1:n)) {
                          zNew <- sin(z^exP + c)
                          z    <- zNew
                        }
                        return(zNew)
                    }, n = 37, c = 1, exP = 3,
                    FUN.VALUE = complex(1))",
                    xlim = limFac*c(-2, 2), ylim = limFac*c(-2, 2),
                    main = "z", axes = TRUE, pType = "pma",
                    xlab = "real", ylab = "imaginary", logBase = 2, nCores = nCores,
                    tempDir = tempDir, res = res, hsvNaN = c(0, 0, 0))



# Irre 2!
windows(29.7/2.54, 1/sqrt(2)*29.7/2.54); res <- 72
# res <- 600; png("Fraktal 1 600 dpi.png", 29.7, 29.7/sqrt(2), units = "cm", res = res)
nCores <- 8
op <- par(mar = c(0, 0, 0, 0), bg = "black")
xlimOrig <- c(-0.43, -0.23)
xRange   <- abs(xlimOrig[2] - xlimOrig[1])*sqrt(2)
xlim     <- c(mean(xlimOrig) - xRange/2, mean(xlimOrig) + xRange/2)

complexFunctionPlot(exprText = "vapply(z, FUN = ff <- function(z, n, c, exP) {
                        for(i in c(1:n)) {
                          zNew <- sin(z^exP + c)
                          z    <- zNew
                        }
                        return(zNew)
                    }, n = 7, c = 1, exP = 3,
                    FUN.VALUE = complex(1))",
                    xlim = xlim, ylim = c(-1.1, -0.9),
                    main = "z", axes = TRUE, pType = "pma",
                    xlab = "real", ylab = "imaginary", logBase = 1.5, nCores = nCores,
                    tempDir = tempDir, res = res, hsvNaN = c(0, 0, 0), xaxs = "i", yaxs = "i")
dev.off()



# Unglaubliches Fraktal
# Irre 2! Breite DINA4, H?he aber 9/16
windows(29.7/2.54, 9/16*29.7/2.54); res <- 72
# res <- 600; png("Fraktal 1 16 zu 9 n = 7 600 dpi.png", 29.7, 9/16*29.7, units = "cm", res = res)
nCores <- 8
op <- par(mar = c(0, 0, 0, 0), bg = "black")
xlimOrig <- c(-0.43, -0.23)
xRange   <- abs(xlimOrig[2] - xlimOrig[1])*16/9
xlim     <- c(mean(xlimOrig) - xRange/2, mean(xlimOrig) + xRange/2)

complexFunctionPlot(exprText = "vapply(z, FUN = ff <- function(z, n, c, exP) {
                        for(i in c(1:n)) {
                          zNew <- sin(z^exP + c)
                          z    <- zNew
                        }
                        return(zNew)
                    }, n = 7, c = 1, exP = 3,
                    FUN.VALUE = complex(1))",
                    xlim = xlim, ylim = c(-1.1, -0.9),
                    main = "z", axes = TRUE, pType = "pma",
                    xlab = "real", ylab = "imaginary", logBase = 1.5, nCores = nCores,
                    tempDir = tempDir, res = res, hsvNaN = c(0, 0, 0), xaxs = "i", yaxs = "i")
text(-0.18, -1.097, "R package viscomplexr by P. Biber", col = "darkgrey", cex = 0.7)
dev.off()



# ------------------------------------------------------------------------------------------------
# Apfelm?nnchen-Gleichung

# Irre 2! Breite DINA4, H?he aber 9/16
windows(29.7/2.54, 9/16*29.7/2.54); res <- 100
# res <- 1200; png("Apfelmaennchen anders herum 1 16 zu 9 1200 dpi n = 37.png", 29.7, 9/16*29.7, units = "cm", res = res)
nCores <- 8
op <- par(mar = c(0, 0, 0, 0), bg = "black")
ylim       <- c(-1.2, 1.2)
yRange     <- abs(ylim[2] - ylim[1])
xlimOrig   <- c(-2, 1)
xRangeOrig <- abs(xlimOrig[2] - xlimOrig[1])
xyfac      <- xRangeOrig / yRange
xRange   <- abs(xlimOrig[2] - xlimOrig[1]) * 16/9 / xyfac
xlim     <- c(mean(xlimOrig) - xRange/2, mean(xlimOrig) + xRange/2)

complexFunctionPlot(exprText = "vapply(z, FUN = ff <- function(z, n) {
                        c  <- z
                        zz <- 0
                        for(i in c(1:n)) {
                          zz <- zz^2 + c
                        }
                        return(zz)
                    }, n = 37,
                    FUN.VALUE = complex(1))",
                    xlim = xlim, ylim = ylim,
                    main = "z", axes = TRUE, pType = "pma",
                    xlab = "real", ylab = "imaginary", logBase = 1.5, nCores = nCores,
                    tempDir = tempDir, res = res, hsvNaN = c(0, 0, 0), xaxs = "i", yaxs = "i")
text(1.3, -1.17, "R package viscomplexr by P. Biber", col = "darkgrey", cex = 0.7)
par(op)
dev.off()


# Ausschnitt 1
windows(29.7/2.54, 9/16*29.7/2.54); res <- 100
# res <- 600; png("Apfelmaennchen Ausschnitt 1 1 16 zu 9 600 dpi n = 37.png", 29.7, 9/16*29.7, units = "cm", res = res)
nCores <- 8
op <- par(mar = c(0, 0, 0, 0), bg = "black")
ylim       <- c(-0.75, 0.0)
yRange     <- abs(ylim[2] - ylim[1])
xlimOrig   <- c(-1.5, 0)
xRangeOrig <- abs(xlimOrig[2] - xlimOrig[1])
xyfac      <- xRangeOrig / yRange
xRange   <- abs(xlimOrig[2] - xlimOrig[1]) * 16/9 / xyfac
xlim     <- c(mean(xlimOrig) - xRange/2, mean(xlimOrig) + xRange/2)
system.time(
complexFunctionPlot(exprText = "vapply(z, FUN = ff <- function(z, n) {
                        c  <- z
                        zz <- 0
                        for(i in c(1:n)) {
                          zz <- zz^2 + c
                        }
                        return(zz)
                    }, n = 37,
                    FUN.VALUE = complex(1))",
                    xlim = xlim, ylim = ylim,
                    main = "z", axes = TRUE, pType = "pma",
                    xlab = "real", ylab = "imaginary", logBase = 1.5, nCores = nCores,
                    tempDir = tempDir, res = res, hsvNaN = c(0, 0, 0), xaxs = "i", yaxs = "i")
) # system.time
text(-1.31, -0.74, "R package viscomplexr by P. Biber", col = "darkgrey", cex = 0.7)
par(op)
dev.off()



# Ausschnitt 2
windows(29.7/2.54, 9/16*29.7/2.54); res <- 100
# res <- 600; png("Apfelmaennchen Ausschnitt 2 1 16 zu 9 600 dpi n = 37.png", 29.7, 9/16*29.7, units = "cm", res = res)
nCores <- 8
op <- par(mar = c(0, 0, 0, 0), bg = "black")
ylim       <- c(0.5, 1.0)
yRange     <- abs(ylim[2] - ylim[1])
xlimOrig   <- c(-0.3, 0.5)
xRangeOrig <- abs(xlimOrig[2] - xlimOrig[1])
xyfac      <- xRangeOrig / yRange
xRange   <- abs(xlimOrig[2] - xlimOrig[1]) * 16/9 / xyfac
xlim     <- c(mean(xlimOrig) - xRange/2, mean(xlimOrig) + xRange/2)
system.time(
  complexFunctionPlot(exprText = "vapply(z, FUN = ff <- function(z, n) {
                        c  <- z
                        zz <- 0
                        for(i in c(1:n)) {
                          zz <- zz^2 + c
                        }
                        return(zz)
                    }, n = 37,
                    FUN.VALUE = complex(1))",
                      xlim = xlim, ylim = ylim,
                      main = "z", axes = TRUE, pType = "pma",
                      xlab = "real", ylab = "imaginary", logBase = 1.5, nCores = nCores,
                      tempDir = tempDir, res = res, hsvNaN = c(0, 0, 0), xaxs = "i", yaxs = "i")
) # system.time
text(-0.27, 0.51, "R package viscomplexr by P. Biber", col = "darkgrey", cex = 0.7)
par(op)
dev.off()



# Ausschnitt 3
windows(29.7/2.54, 9/16*29.7/2.54); res <- 100
# res <- 600; png("Apfelmaennchen Ausschnitt 3 1 16 zu 9 600 dpi n = 52.png", 29.7, 9/16*29.7, units = "cm", res = res)
nCores <- 8
op <- par(mar = c(0, 0, 0, 0), bg = "black")
ylim       <- c(0.25, 0.50)
yRange     <- abs(ylim[2] - ylim[1])
xlimOrig   <- c(-0.75, -0.50)
xRangeOrig <- abs(xlimOrig[2] - xlimOrig[1])
xyfac      <- xRangeOrig / yRange
xRange   <- abs(xlimOrig[2] - xlimOrig[1]) * 16/9 / xyfac
xlim     <- c(mean(xlimOrig) - xRange/2, mean(xlimOrig) + xRange/2)
system.time(
  complexFunctionPlot(exprText = "vapply(z, FUN = ff <- function(z, n) {
                        c  <- z
                        zz <- 0
                        for(i in c(1:n)) {
                          zz <- zz^2 + c
                        }
                        return(zz)
                    }, n = 52,
                    FUN.VALUE = complex(1))",
                      xlim = xlim, ylim = ylim,
                      main = "z", axes = TRUE, pType = "pma",
                      xlab = "real", ylab = "imaginary", logBase = 1.5, nCores = nCores,
                      tempDir = tempDir, res = res, hsvNaN = c(0, 0, 0), xaxs = "i", yaxs = "i")
) # system.time
text(-0.44, 0.255, "R package viscomplexr by P. Biber", col = "darkgrey", cex = 0.7)
par(op)
dev.off()








# ------------------------------------------------------------------------------------------------




# Time test for parallel workers
simplePlot <- function(nCores, res = 150) {
  foreach::registerDoSEQ()
  doParallel::registerDoParallel(nCores)
  windows(10, 10)
  gc()
  tTim <- system.time(
  complexFunctionPlot(exprText = "z", xlim = c(-8, 8), ylim = c(-8, 8),
                      main = "z", axes = TRUE, pType = "p",
                      xlab = "real", ylab = "imaginary", logBase = 2, nCores = nCores, res = res)
  ) # system.time
  dev.off()
  gc()
  return(tTim)
} # simplePlot


nCrs <- c(parallel::detectCores():2)

timeResult <- lapply(nCrs, FUN = simplePlot, res = 600)



# "Sinus von irre gebrochen rational
# Bild soll Bildschirmverhältnis h/b und Breite A4 haben
hoehePx    <- 1080
breitePx   <- 1920
horiA4 <- 29.7
vert   <- horiA4 * hoehePx / breitePx
uGrenzeWerte <- 5 - 8 * horiA4 / vert

x11(width = horiA4/2.54, height = vert/2.54); res <- 150
# windows(horiA4/2.54, vert/2.54); res <- 150
# res <- 600; png("Sinus von irre gebrochen rational 600dpi v2.png", horiA4, vert, units = "cm", res = res)
op <- par(mar = c(0, 0, 0, 0), bg = "black")
phasePortrait(exprText = "sin((1 - z)^3*(z + 2i)^2/(z - 1i)^2)^2", xlim = c(uGrenzeWerte, 5), ylim = c(-5, 3),
                    main = "", axes = FALSE, pType = "pma",
                    # hsvNaN = c(pi/(2*pi), 0.8, 0.6),
                    xlab = "", ylab = "", logBase = exp(1), res = res, xaxs = "i", yaxs = "i",
                    tempDir = tempDir)
par(op)
dev.off()



# Tangens von irre gebrochen rational
# Fuer Facebook - Bild soll Bildschirmverhaeltnis h/b und Breite A4 haben
hoehePx    <- 1080
breitePx   <- 1920
horiA4 <- 29.7
vert   <- horiA4 * hoehePx / breitePx
uGrenzeWerte <- 5 - 8 * horiA4 / vert
nCores  <- 8

# x11(width = horiA4/2.54, height = vert/2.54); res <- 237
# windows(horiA4/2.54, vert/2.54); res <- 237
# res <- 600; png("Tangens von irre gebrochen rational FB horiA4 16zu9 600dpi.png", horiA4, vert, units = "cm", res = res)
res <- 600; png("Tangens von irre gebrochen rational Ubu FB horiA4 16zu9 600dpi.png", horiA4, vert, units = "cm", res = res)
op <- par(mar = c(0, 0, 0, 0), bg = "black")
phasePortrait(exprText = "tan((1 - z)^3*(z + 2i)^2/(z - 1i)^2)^2",
                                 xlim = c(uGrenzeWerte, 5), ylim = c(-5, 3),
                                 main = "", axes = FALSE, pType = "pma",
                                 xlab = "", ylab = "", logBase = exp(1),
                                 darkestShade = 0,
                                 res = res, xaxs = "i", yaxs = "i", tempDir = tempDir, nCores = nCores,
                                 gamma = 1, pi2Div = 12)
text(x = 4.5, y = -4.85, labels = "Peter Biber", col = "grey40", cex = 0.9)
par(op)
dev.off()


# macht gar nichts
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

unregister()




