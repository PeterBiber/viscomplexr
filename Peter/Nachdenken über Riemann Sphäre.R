
# Ich baue mir eine Matrix, die einen Ausschnitt aus der komplexen Zahlenebene
# darstellen soll.

mReal <- matrix(rep(seq(-4/3, 4/3,  1/3), 9), 9, 9, byrow = TRUE)
mImag <- matrix(rep(seq(4/3, -4/3, -1/3), 9), 9, 9, byrow = FALSE)
mComp <- matrix(complex(real = mReal, imaginary = mImag), 9, 9)
mCompInv <- 1/mComp

abs(mCompInv)

