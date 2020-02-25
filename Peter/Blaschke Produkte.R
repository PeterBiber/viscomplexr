# -----------------------------------------------------------------------------
# Versuche mit Blaschke-Produkten
# -----------------------------------------------------------------------------
library(viscomplexr)

# setwd("E:/Peter/Projekte/viscomplexr/Peter")
# tempDir <- "E:/temp/VisComplexFiles"

setwd("/media/peter/Rindskanoppel/Peter/Projekte/viscomplexr/Peter")
tempDir <- "/media/peter/Rindskanoppel/temp/VisComplexFiles"

# -----------------------------------------------------------------------------

x11(width = 8, height = 8)
# n <- 20
# a <- complex(n, modulus = runif(n), argument = 2*pi*runif(n))
phasePortrait("vapply(z, function(z, a){
                return(prod(abs(a)/a * (a-z)/(1-Conj(a)*z)))
               }, a = c(0.12152611+0.06171533i,  0.53730315+0.32797530i,
                        0.35269601-0.53259644i, -0.57862039+0.33328986i,
                       -0.94623221+0.06869166i, -0.02392968-0.21993132i,
                        0.04060671+0.05644165i,  0.15534449-0.14559097i,
                        0.32884452-0.19524764i,  0.58631745+0.05218419i,
                        0.02562213+0.36822933i, -0.80418478+0.58621875i,
                       -0.15296208-0.94175193i, -0.02942663+0.38039250i,
                       -0.35184130-0.24438324i, -0.09048155+0.18131963i,
                        0.63791697+0.47284679i,  0.25651928-0.46341192i,
                        0.04353117-0.73472528i, -0.04606189+0.76068461i),
              FUN.VALUE = complex(1))", pType = "p",
              xlim = c(-1.7, 1.7), ylim = c(-1.7, 1.7), tempDir = tempDir, pi2Div = 9,
              xlab = "real", ylab = "imaginary", darkestShade = 0)

# -----------------------------------------------------------------------------

x11(width = 12, height = 8); res <- 150
# n <- 20
# a <- complex(n, modulus = runif(n), argument = 2*pi*runif(n))
    # res <- 600; png("TangensBlaschke.png", 24, 15, units = "cm", res = res)
    op <- par(bg = "black", mar = c(0, 0, 0, 0))
    phasePortrait("vapply(z, function(z, a){
                    return(tan(prod(abs(a)/a * (a-z)/(1-Conj(a)*z))))
                   }, a = c(0.12152611+0.06171533i,  0.53730315+0.32797530i,
                            0.35269601-0.53259644i, -0.57862039+0.33328986i,
                           -0.94623221+0.06869166i, -0.02392968-0.21993132i,
                            0.04060671+0.05644165i,  0.15534449-0.14559097i,
                            0.32884452-0.19524764i,  0.58631745+0.05218419i,
                            0.02562213+0.36822933i, -0.80418478+0.58621875i,
                           -0.15296208-0.94175193i, -0.02942663+0.38039250i,
                           -0.35184130-0.24438324i, -0.09048155+0.18131963i,
                            0.63791697+0.47284679i,  0.25651928-0.46341192i,
                            0.04353117-0.73472528i, -0.04606189+0.76068461i),
                  FUN.VALUE = complex(1))", pType = "pma", res = res,
                  xlim = c(-4, 2), ylim = c(-2, 2), tempDir = tempDir, pi2Div = 9,
                  xlab = "real", ylab = "imaginary", darkestShade = 0, axes = FALSE)
    par(op)
    dev.off()

# -----------------------------------------------------------------------------

x11(width = 12, height = 8); res <- 150
# n <- 20
# a <- complex(n, modulus = runif(n), argument = 2*pi*runif(n))
# res <- 600; png("TangensBlaschkeUmk.png", 24, 15, units = "cm", res = res)
op <- par(bg = "black", mar = c(0, 0, 0, 0))
phasePortrait("vapply(z, function(z, a){
                return(tan(prod(abs(a)/a * (a-z)/(1-Conj(a)*z))))
               }, a = c(0.12152611+0.06171533i,  0.53730315+0.32797530i,
                        0.35269601-0.53259644i, -0.57862039+0.33328986i,
                       -0.94623221+0.06869166i, -0.02392968-0.21993132i,
                        0.04060671+0.05644165i,  0.15534449-0.14559097i,
                        0.32884452-0.19524764i,  0.58631745+0.05218419i,
                        0.02562213+0.36822933i, -0.80418478+0.58621875i,
                       -0.15296208-0.94175193i, -0.02942663+0.38039250i,
                       -0.35184130-0.24438324i, -0.09048155+0.18131963i,
                        0.63791697+0.47284679i,  0.25651928-0.46341192i,
                        0.04353117-0.73472528i, -0.04606189+0.76068461i),
              FUN.VALUE = complex(1))", pType = "pma", res = res,
              xlim = c(-4, 2), ylim = c(-2, 2), tempDir = tempDir, pi2Div = 9, invertFlip = TRUE,
              xlab = "real", ylab = "imaginary", darkestShade = 0, axes = FALSE)
par(op)
dev.off()

# -----------------------------------------------------------------------------

x11(width = 24/2.54, height = 16/2.54); res <- 150
# res <- 300; png("Blaschke 77a sig.png", 24, 16, units = "cm", res = res)
op <- par(mar = c(1, 1, 1, 1), bg = "black")
n <- 77
a <- complex(n, modulus = runif(n), argument = 2*pi*runif(n))
a <- vector2String(a)
FUN <- paste("vapply(z, function(z, a){
                    return(prod(abs(a)/a * (a-z)/(1-Conj(a)*z)))
                   }, a =", a,
             ", FUN.VALUE = complex(1))", sep = "")

phasePortrait(FUN, pType = "pma", axes = FALSE, res = res,
              xlim = c(-3, 3), ylim = c(-2.0, 2.0), tempDir = tempDir, pi2Div = 9,
              xlab = "real", ylab = "imaginary", darkestShade = 0)
# mtext("R package viscomplexr by P. Biber                 ",
#       side = 1, line = 3.5, adj = 1, col = "grey25", cex = 1)
par(op)
dev.off()


# -----------------------------------------------------------------------------

x11(width = 24/2.54, height = 16/2.54); res <- 150
# res <- 300; png("Blaschke nur 7.png", 24, 16, units = "cm", res = res)
op <- par(mar = c(1, 1, 1, 1), bg = "black")
n <- 7
a <- complex(n, modulus = runif(n), argument = 2*pi*runif(n))
a <- vector2String(a)
FUN <- paste("vapply(z, function(z, a){
                    return(prod(abs(a)/a * (a-z)/(1-Conj(a)*z)))
                   }, a =", a,
             ", FUN.VALUE = complex(1))", sep = "")
phasePortrait(FUN, pType = "p", axes = FALSE, res = res,
              xlim = c(-3, 3), ylim = c(-2.0, 2.0), tempDir = tempDir, pi2Div = 9,
              xlab = "real", ylab = "imaginary", darkestShade = 0)
# riemannMask()
# mtext("R package viscomplexr by P. Biber                 ",
#       side = 1, line = 3.5, adj = 1, col = "grey25", cex = 1)
par(op)
dev.off()

# -----------------------------------------------------------------------------

x11(width = 24/2.54, height = 16/2.54); res <- 150
# res <- 300; png("Blaschke Spiral.png", 24, 16, units = "cm", res = res)
op <- par(mar = c(1, 1, 1, 1), bg = "black")
n  <- 36
m  <- c(1:n)
a <- sapply(m, function(m, nmax) {
        m/nmax * (cos(4.7*pi*m/nmax) + 1i*sin(4.7*pi*m/nmax))
    }, nmax = n)
a <- vector2String(a)
FUN <- paste("vapply(z, function(z, a){
                    return(prod(abs(a)/a * (a-z)/(1-Conj(a)*z)))
                   }, a =", a,
             ", FUN.VALUE = complex(1))", sep = "")
phasePortrait(FUN, pType = "p", axes = FALSE, res = res,
              xlim = c(-6, 6), ylim = c(-4.0, 4.0), tempDir = tempDir, pi2Div = 9,
              xlab = "real", ylab = "imaginary", darkestShade = 0)
# riemannMask()
# mtext("R package viscomplexr by P. Biber                 ",
#       side = 1, line = 3.5, adj = 1, col = "grey25", cex = 1)
par(op)
dev.off()

# -----------------------------------------------------------------------------
