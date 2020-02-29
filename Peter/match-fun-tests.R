# Versuche zum Einrichten eines verbesserten Funktionscalls

alfred <- function(FUN, z) {
  ff  <- tryCatch(match.fun(FUN), error = function(e) NULL)
  if(is.null(ff)) erg <- NULL
  else            erg <- ff(z)
  return(erg)
}

alfred(sepp, 3)
# alfred("function(z) 3+z", 3) # Error
alfred(mean, 3)
alfred("mean", 3)
alfred("schwurbel", 3)

trottel <- function(what) {
  tt <- tryCatch(match.fun(what), error = function(err) return(NULL))
  return(tt)
}


trottel(mean)
trottel("mean")
trottel("paffhans")
trottel(paffhans)


sepp <- makeFunktionFromInput(FUN = "z^3+5", moreArgs = list(a = 12, bb = "Volltrottel", sauHund = 21))
formals(sepp)
body(sepp)
environment(sepp)


x11(width = 8, height = 8)
phasePortrait("z^3 + 1", xlim = c(-5, 5), ylim = c(-5, 5))

x11(width = 8, height = 8)
phasePortrait(function(z) z^3 + 1, xlim = c(-5, 5), ylim = c(-5, 5))

sepp <- function(k) k^3
x11(width = 8, height = 8)
phasePortrait(sepp, xlim = c(-5, 5), ylim = c(-5, 5))

sepp <- function(k) k^3
x11(width = 8, height = 8)
phasePortrait("sepp", xlim = c(-5, 5), ylim = c(-5, 5))


# Makes obviously no sense to fiddle around with pointers
# to functions.
pSepp <- newPointer(sepp)
pSepp$value

system.time(
  do.call(pSepp$value, list(c(1:2e7)))
)

system.time(
  do.call(sepp, list(1:2e7))
)
pSepp <- NULL


system.time(
  argt <- c(1:2e7),
  vapply(argt, function(x, pFun) {
    x <- list(x)
    do.call(pFun$value, x)
  }, FUN.VALUE = numeric(1), pFun = pSepp)
)
# ------

x11(width = 16, height = 8)
op <- par(mfrow = c(1, 2))
res <- 150
phasePortrait("(z - 1)/((-1/2 - z)^2*(z - 1i)^2)",
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
              res = res)
riemannMask()
phasePortrait("(z - 1)/((-1/2 - z)^2*(z - 1i)^2)",
              xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
              invertFlip = TRUE, res = res)
riemannMask()
par(op)

# ------







