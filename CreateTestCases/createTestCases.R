# ------------------------------------------------------------------------------
# This script contains a few calls for phasePortrait that produce test
# w-Files that serve as references for automated tests
# ------------------------------------------------------------------------------

# Test case 1
tempDir <- "tests/testthat"
phasePortrait("(2-z)^2*(-1i+z)^3*(4-3i-z)/((2+2i+z)^4)",
              xlim = c(-4, 4), ylim = c(-4, 4), invertFlip = FALSE,
              tempDir = tempDir, deleteTempFiles = FALSE,
              noScreenDevice = TRUE)


# Test case 2 as above but with invertFlip = TRUE
tempDir <- "tests/testthat"
phasePortrait("(2-z)^2*(-1i+z)^3*(4-3i-z)/((2+2i+z)^4)",
              xlim = c(-4, 4), ylim = c(-4, 4), invertFlip = TRUE,
              tempDir = tempDir, deleteTempFiles = FALSE,
              noScreenDevice = TRUE)


# Test case 3
# User function with additional default arguments which are _not_ specified
# in the call to phasePortrait
jacobiTheta <- function(z, tau = 1i, nIter = 30) {
  k <- c(1:nIter)
  q <- exp(pi*1i*tau)
  g <- exp(2*pi*1i*z)
  return(1 + sum(q^(k^2)*g^k + q^(k^2)*(1/g)^k))
}
tempDir <- "tests/testthat"
phasePortrait("jacobiTheta",
              xlim = c(-2, 2), ylim = c(-2, 2), invertFlip = FALSE,
              tempDir = tempDir, deleteTempFiles = FALSE,
              noScreenDevice = TRUE)


# Test case 4
# User function with additional default arguments which _are_ specified
# in the call to phasePortrait
tempDir <- "tests/testthat"
phasePortrait("jacobiTheta", moreArgs = list(tau = 1i/2 - 1/4, nIter = 30),
              xlim = c(-2, 2), ylim = c(-2, 2), invertFlip = FALSE,
              tempDir = tempDir, deleteTempFiles = FALSE,
              noScreenDevice = TRUE)




