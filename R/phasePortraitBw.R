# R package viscomplexr - phase portraits of functions in the
# complex number plane
# Copyright (C) 2020 Peter Biber
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>



# -----------------------------------------------------------------------------
# function phaseModColBw

# Calculates a hex two-color array based on an array of complex numbers, both
# arrays handed over as pointers.

# The function takes into account only the modulus values. The modulus of a
# complex number is attributed to a zone based on the input parameter logBase,
# and either assigned the first or the second value of the input variable
# bwCols. Only in cases where the modulus cannot be determined (NaNs or Inf),
# the third color in bwCols is used.

# In more detail, for an input number's modulus, the logarithm with base
# logBase is calculated and cut down to the next lower integer value. If this
# is an even number, the first color of bwCols is taken. In case of an odd
# number, the second color is used.

phaseModColBw <- function(pCompArr,
                          pBwCol,
                          logBase = exp(2*pi/18),
                          bwCols = c("black", "grey95", "grey")) {

  hexCols <- sapply(bwCols,
                    function(bwc) rgb(t(col2rgb(bwc)), maxColorValue = 255))

  dims    <- dim(pCompArr$value)

  intMod  <- floor(log(Mod(pCompArr$value), logBase))

  intIdx  <- intMod %% 2 + 1
  intIdx  <- ifelse(is.nan(intIdx), 3, intIdx)

  pBwCol$value <- array(hexCols[intIdx], dims)

  return(pBwCol)

} # phaseModColBw

# -----------------------------------------------------------------------------
# function phaseAngColBw

# Calculates a hex two-color array based on an array of complex numbers, both
# arrays handed over as pointers.

# The function takes into account only the argument values. The argument of a
# complex number is attributed to a zone based on the input parameters pi2Div
# and argOffset. Then, it is either assigned to the first or the second value
# of the input variable bwCols. Only in cases where the argeument cannot be
# determined (NaNs) the third color in bwCols is used.

# In more detail, the full angle (2*pi) is divided into p2Div zones, which are
# numbered from 0 to pi2Div - 1 with increasing angle. Even and odd zone numbers
# are attributed the first and the second color in bwCols, respectively.
# Usually, the input parameter pi2Div should be an even number in order to avoid
# the first and the last zone having the same color.

phaseAngColBw <- function(pCompArr,
                          pBwCol,
                          pi2Div = 18,
                          argOffset = 0,
                          bwCols = c("black", "grey95", "grey")) {

  hexCols <- sapply(bwCols,
                    function(bwc) rgb(t(col2rgb(bwc)), maxColorValue = 255))

  dims    <- dim(pCompArr$value)
  argmt   <- Arg(pCompArr$value)
  intArg  <- floor(ifelse(argmt - argOffset < 0, argmt + 2*pi, argmt) /
                     (2 * pi / pi2Div))

  intIdx  <- intArg %% 2 + 1
  intIdx  <- ifelse(is.nan(intIdx), 3, intIdx)

  pBwCol$value <- array(hexCols[intIdx], dims)

  return(pBwCol)

} # phaseAngColBw

# -----------------------------------------------------------------------------
# function phaseModAngColBw

# Calculates a hex two-color array based on an array of complex numbers, both
# arrays handed over as pointers.

# The function takes into account the modulus and the argument values and
# colors the resulting grid in a chessboard-like alternation using the first
# and the second color in the input variable bwCols. Only in cases where the
# modulus or the argument cannot be determined (NaNs or Inf), the third color
# in bwCols is used.

# In more detail, for an input number's modulus, the logarithm with base
# logBase is calculated and cut down to the next lower integer value. For the
# argument, the full angle (2*pi) is divided into p2Div zones, which are
# numbered from 0 to pi2Div - 1 with increasing angle. The sum of both integers
# is taken, and if it is an even or an odd number, the first or the second
# color from bwCols is used, respectively.

phaseModAngColBw <- function(pCompArr,
                             pBwCol,
                             pi2Div  = 18,
                             logBase = exp(2*pi/pi2Div),
                             argOffset = 0,
                             bwCols = c("black", "grey95", "grey")) {

  hexCols <- sapply(bwCols,
                    function(bwc) rgb(t(col2rgb(bwc)), maxColorValue = 255))

  dims    <- dim(pCompArr$value)
  argmt   <- Arg(pCompArr$value)
  intArg  <- floor(ifelse(argmt - argOffset < 0, argmt + 2*pi, argmt) /
                     (2 * pi / pi2Div))
  intMod  <- floor(log(Mod(pCompArr$value), logBase))

  intIdx  <- (intArg + intMod) %% 2 + 1
  intIdx  <- ifelse(is.nan(intIdx), 3, intIdx)

  pBwCol$value <- array(hexCols[intIdx], dims)

  return(pBwCol)

} # phaseModAngColBw

# -----------------------------------------------------------------------------
# Function complexArrayPlotBw

# Very much like the function complexArrayPlot, but tailored for two-color
# plots to be created by calling phasePortraitBw.

# Displays an array of complex numbers in an existing plot.
# In order to do so,the temporary files that together form the array are
# read from disk one by one, but each one is processed in a parallel loop.
# The resulting array of hex color values is finally plotted as
# a raster image.

complexArrayPlotBw <- function(zMetaInfrm,
                               xlim,
                               ylim,
                               pType = "ma",
                               invertFlip = FALSE,
                               pi2Div = 18,
                               logBase = exp(2*pi/pi2Div),
                               argOffset = 0,
                               bwCols = c("black", "grey95", "grey"),
                               asp = 1,
                               xlab = "", ylab = "",
                               verbose,
                               ...) {

  # Set up plot
  plot(NULL, xlim = xlim, ylim = ylim, asp = asp, xlab = xlab, ylab = ylab, ...)

  # Define call to color transformation function depending user's
  # choice of pType
  colCmd <- switch(pType,
                   "m"  = "phaseModColBw(pListCompArr[[i]],
                                         pBwCol,
                                         logBase = logBase,
                                         bwCols = bwCols)",

                   "a"  = "phaseAngColBw(pListCompArr[[i]],
                                         pBwCol,
                                         pi2Div = pi2Div,
                                         argOffset = argOffset,
                                         bwCols = bwCols)",

                   "ma" = "phaseModAngColBw(pListCompArr[[i]],
                                            pBwCol,
                                            pi2Div = pi2Div,
                                            logBase = logBase,
                                            argOffset = argOffset,
                                            bwCols = bwCols)"
  ) # switch


  # Obtain the names of the files to load and process
  zMetaInfrm$metaZ$wFileNames <- paste(zMetaInfrm$tempDir,
                                       zMetaInfrm$metaZ$wFileNames, sep = "/")

  # Run the color transformation function over each file
  pBwCol <- lapply(c(1:nrow(zMetaInfrm$metaZ)),
    function(i, zMetaInfrm, colCmd) {

      if(verbose) cat("\n.transforming block", i, "... ")
      # load a block (will soon become a list of pointers, hence the name)
      pListCompArr  <- get(load(zMetaInfrm$metaZ[i,]$wFileNames))
      # split it for parallel processing
      nCores   <- getDoParWorkers()
      uplow    <- verticalSplitIndex(nrow(pListCompArr), nCores)

      # here's the actual splitting, pListCompArr becomes a list of pointers
      pListCompArr  <- lapply(uplow, FUN = function(uplow, pListCompArr) {
      nwPtr <- newPointer(pListCompArr[c(uplow[1]:uplow[2]),])
      # if the split result has only one line, it will automatically become a
      # vector, which is undesired, because functions coming later require it
      # as a two-dimensional array. This is made sure here.
      if(uplow[1] == uplow[2]) {
        dim(nwPtr$value) <- c(1, length(nwPtr$value))
      }
      return(nwPtr)
    }, pListCompArr = pListCompArr)

    # Parallel loop transforming the chunks into a color raster each;
    # giving back a list of pointers to the rasters
    if(verbose) cat("parallel loop starting ... ")
      pBwCol <- foreach(i = c(1:length(pListCompArr)),
                        .export  = c("phaseColhsv",
                                     "phaseModColhsv",
                                     "phaseAngColhsv",
                                     "phaseModAngColBw",
                                     "bwCols",
                                     "logBase",
                                     "pi2Div",
                                     "newPointer",
                                     "argOffset"),
                        .combine = c) %dopar% {

        pBwCol  <- newPointer(NULL)
        eval(parse(text = colCmd))      # Does not require a return value,
        # changes color array via pointer
        pListCompArr[[i]]$value <- NULL # Reduced here, but removed after
                                        # the foreach loop
        return(pBwCol)
      } # foreach
      if(verbose) cat("done.")

      # Remove the original list of array pointers
      rm(pListCompArr)

      # Combine the color arrays in the value of the first pointer.
      # Free the others (rbindArraysbyPointer).
      #   Enforce (one-element-) list in case there is only one value
      #   (i.e. if foreach loop was executed sequentially, one core only)
      if(length(pBwCol) == 1) pBwCol <- list(pBwCol)
      pBwCol <- rbindArraysbyPointer(pBwCol)

      return(pBwCol)
    }, # function in lapply
    zMetaInfrm = zMetaInfrm, colCmd = colCmd
  ) # lapply

  # Now combine all blocks into the big raster ...
  if(verbose) cat("\nCombine color rasters ... ")
  pBwCol <- rbindArraysbyPointer(pBwCol)
  if(verbose) cat("done.\n")

  # ... and plot it
  if(verbose) cat("Plotting raster image ... ")
  rasterImage(as.raster(pBwCol$value), xlim[1], ylim[1], xlim[2], ylim[2])
  if(verbose) cat("done.\n")

  pBwCol$value <- NULL
  rm(pBwCol)

  return(NULL)

} # complexArrayPlotBw

# -----------------------------------------------------------------------------


#' Black-and-white phase portraits
#'
#' @param FUN
#' @param moreArgs
#' @param xlim
#' @param ylim
#' @param invertFlip
#' @param res
#' @param blockSizePx
#' @param tempDir
#' @param nCores
#' @param pType
#' @param pi2Div
#' @param logBase
#' @param argOffset
#' @param bwCols
#' @param asp
#' @param deleteTempFiles
#' @param noScreenDevice
#' @param autoDereg
#' @param verbose
#' @param ...
#'
#' @return
#'
#' @export
#'
#' @examples
#'
phasePortraitBw <- function(FUN, moreArgs = NULL, xlim, ylim,
                            invertFlip = FALSE,
                            res = 150,
                            blockSizePx = 2250000,
                            tempDir = NULL,
                            nCores = parallel::detectCores(),
                            pType = "ma",
                            pi2Div = 18,
                            logBase = exp(2*pi/pi2Div),
                            argOffset = 0,
                            bwCols = c("black", "grey95", "grey"),
                            asp = 1,
                            deleteTempFiles = TRUE,
                            noScreenDevice = FALSE,
                            autoDereg = FALSE,
                            verbose = TRUE,
                            ...) {

  # Bring the user's function definition in workable form
  compFun <- makeFunctionFromInput(FUN, moreArgs)
  if(is.null(compFun)) stop("\nFUN cannot be interpreted.")

  # Calculate pixel array size from plot region size in inch and the plot
  # range for the function given with xlim and ylim

  ## par("pin"): plot region size in inch; first is horizontal
  ## if noScreenDevice, region is set to 1 x 1 inch
  if(!noScreenDevice) regionPi  <- par("pin")
  else                regionPi  <- c(1, 1)

  xRange    <- abs(xlim[2] - xlim[1])
  yRange    <- abs(ylim[2] - ylim[1])

  yxRangeRatio <- yRange      / xRange
  yxPinchRatio <- regionPi[1] / regionPi[2]

  if(yxRangeRatio < yxPinchRatio) { # height is limiting
    heightPx <- res * regionPi[2]
    widthPx  <- res * regionPi[2] / yxRangeRatio
  } # if
  else { # width is limiting
    widthPx  <- res * regionPi[1]
    heightPx <- res * regionPi[1] * yxRangeRatio
  } #else

  widthPx  <- round(widthPx)
  heightPx <- round(heightPx)

  # In case of invertFlip == TRUE swap xlim
  if(invertFlip) {
    xlim  <- c(xlim[2], xlim[1])
  } # if invertFlip

  # Register parallel Cluster if required or change number of workers
  nWorkers   <- getDoParWorkers() # number registered
  availCores <- detectCores()     # number available
  nCores     <- min(max(nCores, 1), availCores) # register at least 1 :)
  # and not more than available
  if(nCores != 1) {
    if(nWorkers != nCores) {
      if(verbose) cat("\nRegistering parallel workers ... ")
      registerDoSEQ()    # Unregister parallel for the sake of safety before
      registerDoParallel(cores = nCores) # register with new number of cores
      if(verbose) cat(nCores, "parallel workers registered ...")
    }
    else {
      if(verbose)
        cat("\n", nCores, " parallel workers previously registered ...",
            sep = "")
    }
  }
  # Only one core desired
  else {
    registerDoSEQ()
    if(verbose)
      cat("\nnCores set to 1.",
          "Parallel loops will be executed sequentially ...")
  }

  # Make pixelwise array of z-Values (input values to function)
  if(verbose) cat("\nBuilding z plane array ...")
  if(is.null(tempDir)) tempDir <- tempdir()
  zMetaInfrm <- buildArray(widthPx, heightPx, xlim, ylim, blockSizePx, tempDir,
                           verbose)

  # This is where it really happens
  if(verbose) cat("\nEvaluation loop starting ... ")
  zMetaInfrm$metaZ$wFileNames <- vapply(c(1:nrow(zMetaInfrm$metaZ)),
    function(i, zMetaInfrm, compFun, moreArgs) {

      if(verbose) cat("\n.processing block", i, "... ")
      fileName       <- paste(zMetaInfrm$tempDir,
                              zMetaInfrm$metaZ[i,]$fileName, sep = "/")
      z              <- get(load(fileName))

      # Split z vertically (by rows) into nCores chunks to be processed
      # in parallel
      # - here's some pre-work
      uplow <- verticalSplitIndex(dim(z)[1], nCores)

      # - here's the actual splitting, z becomes a list
      z <- lapply(uplow, FUN = function(uplow, z) {
        return(z[c(uplow[1]:uplow[2]),])
      }, z = z)

      # Construct function call to be evaluated inside the parallel loop
      if(is.null(moreArgs)) {
        vCall <- "vapply(z[[i]], compFun, FUN.VALUE = complex(1))"
      }
      else {
        vCall <- paste("vapply(z[[i]], compFun, FUN.VALUE = complex(1),",
                       paste(names(moreArgs), "=", moreArgs, collapse = ","),
                       ")")
      }
      vCall <- parse(text = vCall)

      # Run the evaluation parallel on each core and put it together again
      if(verbose) cat("parallel loop starting ... ")

      w <- foreach(i = c(1:length(z)), .combine = rbind,
        .export = c("invertFlip", "compFun")) %dopar% {
        # Make sure dimensions are correct, because
        # one-line arrays can become vectors mysteriously ...
        if(length(dim(z[[i]])) < 2) dims <- c(1, length(z[[i]]))
        else                        dims <- dim(z[[i]])
        if(invertFlip) z[[i]]            <- Conj(1 / z[[i]])
        array(eval(vCall), dim = dims)
      } # foreach i
      if(verbose) cat("done.")

      rm(z) # discard z array

      wFileName <- paste(formatC(
          zMetaInfrm$metaZ[i,]$lower,
          width =
            trunc(log10(zMetaInfrm$metaZ$lower[nrow(zMetaInfrm$metaZ)])) + 1,
          flag = "0"
        ), # formatC
        "wmat", zMetaInfrm$rndCode, ".RData", sep = "")

      save(w, file = paste(zMetaInfrm$tempDir, wFileName, sep = "/"))
      rm(w)

      return(wFileName)
    }, # function FUN

    FUN.VALUE = character(1),
    zMetaInfrm = zMetaInfrm, compFun = compFun, moreArgs = moreArgs
  ) # vapply

  # Transform into color values and plot it
  if(!noScreenDevice) {
    if(verbose) cat("\nTransforming function values into colors ...")
    complexArrayPlotBw(zMetaInfrm, xlim, ylim, pType, invertFlip,
                       pi2Div, logBase, argOffset, bwCols,
                       verbose = verbose, ...)
  } # if(!noScreenDevice)
  else if(verbose) cat("\nNo plot is made (explicit wish of the user) ...")

  # Delete all temporary files ... or not
  if(deleteTempFiles) {
    if(verbose) cat("Deleting temporary files ... ")
    filesToDelete <- paste(zMetaInfrm$tempDir,
                           c(as.character(zMetaInfrm$metaZ$fileNames),
                             as.character(zMetaInfrm$metaZ$wFileNames)),
                           sep = "/")
    unlink(filesToDelete)
    if(verbose) cat("done.\n")
  } else {
    if(verbose)
      cat("\nTemporary files are NOT deleted (explicit wish of the user).\n")
  } # else (temp files ore not deleted)

  # If a parallel backend has been registered, keep or register a sequential
  # backend dependent on user settings
  if(nCores > 1) {
    if(!autoDereg) {
      if(verbose) cat("\nParallel backend with", nCores,
                      "cores remains registered for convenience.")
      if(verbose)
        cat("\nCan be de-registered manually with",
            "'foreach::registerDoSEQ()'.\n")
    }
    else {
      foreach::registerDoSEQ()
      if(verbose) cat("\nSequential backend registered again.\n")
    }
  } # if nCores > 1

  invisible(TRUE) # For test purposes
} # phasePortraitBw

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


