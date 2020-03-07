# -----------------------------------------------------------------------------
# Test that phasePortrait produces the correct numerical output.
# -----------------------------------------------------------------------------

# Names of RData files containing the reference cases.
# These files will not be deleted by the function cleanUp, which is
# called before each test.
referenceFileNames <- c("1wmatCase001.RData", "1wmatCase002.RData",
                        "1wmatCase003.RData", "1wmatCase004.RData")

# function cleanUp
# deletes all non-reference .RData files
cleanUp <- function(refFileNames) {
  existRData         <- dir(pattern = "\\.RData$")
  notReference       <- existRData[!(existRData %in% refFileNames)]
  unlink(notReference)
}


# function loadNonRef
# loads the only .RData file with "wmat" in its name which can exist
# in the tests/testhat directory without being a reference file
loadNonRef <- function(refFileNames) {
  existRData         <- dir(pattern = "*wmat*.*.RData$")
  notReference       <- existRData[!(existRData %in% refFileNames)]
  get(load(notReference))
}



# Test cases defined as functions

# Test case 1:
# A rational function given as single string
testCase1 <- function(refFileNames) {
  cleanUp(refFileNames)
  phasePortrait("(2-z)^2*(-1i+z)^3*(4-3i-z)/((2+2i+z)^4)",
                xlim = c(-4, 4), ylim = c(-4, 4),
                invertFlip = FALSE, res = 150,
                deleteTempFiles = FALSE,
                noScreenDevice = TRUE,
                nCores = 1)
  referenceWmat <- get(load("1wmatCase001.RData"))
  actualWmat    <- loadNonRef(refFileNames)
  cleanUp(refFileNames)
  rslt          <- all.equal(referenceWmat, actualWmat)
  rm(referenceWmat, actualWmat)
  return(rslt)
}


# Test case 2:
# A rational function given as single string, but with invertFlip = TRUE
testCase2 <- function(refFileNames) {
  cleanUp(refFileNames)
  phasePortrait("(2-z)^2*(-1i+z)^3*(4-3i-z)/((2+2i+z)^4)",
                xlim = c(-4, 4), ylim = c(-4, 4),
                invertFlip = TRUE, res = 150,
                deleteTempFiles = FALSE,
                noScreenDevice = TRUE,
                nCores = 1)
  referenceWmat <- get(load("1wmatCase002.RData"))
  actualWmat    <- loadNonRef(refFileNames)
  cleanUp(refFileNames)
  rslt          <- all.equal(referenceWmat, actualWmat)
  rm(referenceWmat, actualWmat)
  return(rslt)
}


# Test case 3
# User function with additional default arguments which are _not_ specified
# in the call to phasePortrait
testCase3 <- function(refFileNames) {

  jacobiTheta <- function(z, tau = 1i, nIter = 30) {
    k <- c(1:nIter)
    q <- exp(pi*1i*tau)
    g <- exp(2*pi*1i*z)
    return(1 + sum(q^(k^2)*g^k + q^(k^2)*(1/g)^k))
  }

  cleanUp(refFileNames)
  phasePortrait(jacobiTheta,
                xlim = c(-2, 2), ylim = c(-2, 2),
                invertFlip = FALSE, res = 150,
                deleteTempFiles = FALSE,
                noScreenDevice = TRUE,
                nCores = 1)
  referenceWmat <- get(load("1wmatCase003.RData"))
  actualWmat    <- loadNonRef(refFileNames)
  cleanUp(refFileNames)
  rslt          <- all.equal(referenceWmat, actualWmat)
  rm(referenceWmat, actualWmat)
  return(rslt)
}


# Test case 4
# User function with additional default arguments which are _not_ specified
# in the call to phasePortrait
testCase4 <- function(refFileNames) {

  jacobiTheta <- function(z, tau = 1i, nIter = 30) {
    k <- c(1:nIter)
    q <- exp(pi*1i*tau)
    g <- exp(2*pi*1i*z)
    return(1 + sum(q^(k^2)*g^k + q^(k^2)*(1/g)^k))
  }

  cleanUp(refFileNames)
  phasePortrait(jacobiTheta,
                moreArgs = list(tau = 1i/2 - 1/4, nIter = 30),
                xlim = c(-2, 2), ylim = c(-2, 2),
                invertFlip = FALSE, res = 150,
                deleteTempFiles = FALSE,
                noScreenDevice = TRUE,
                nCores = 1)
  referenceWmat <- get(load("1wmatCase004.RData"))
  actualWmat    <- loadNonRef(refFileNames)
  cleanUp(refFileNames)
  rslt          <- all.equal(referenceWmat, actualWmat)
  rm(referenceWmat, actualWmat)
  return(rslt)
}



# The actual tests
test_that("phasePortrait produces correct numerical output", {
  expect_true(testCase1(referenceFileNames))
  expect_true(testCase2(referenceFileNames))
  expect_true(testCase3(referenceFileNames))
  expect_true(testCase4(referenceFileNames))
})

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------


