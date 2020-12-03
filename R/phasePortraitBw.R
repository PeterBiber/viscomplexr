# -----------------------------------------------------------------------------

# Construction site for black-white phase portraits

# -----------------------------------------------------------------------------

phaseModAngColBw <- function(pCompArr,
                             pHsvCol,
                             logBase = 2,
                             pi2Div = 24,
                             argOffset = 0,
                             bwCols = c("black", "white"),
                             hsvNaN = c(0, 0, 0.5)) {

  names(hsvNaN) <- c("h", "s", "v")
  dims    <- dim(pCompArr$value)
  argmt   <- Arg(pCompArr$value)
  h       <- ifelse(is.nan(argmt), hsvNaN["h"],
                    ifelse(argmt < 0, argmt + 2*pi, argmt)/(2 * pi))

  vMod    <- Mod(pCompArr$value)
  vMod    <- ifelse(is.nan(pCompArr$value), hsvNaN["v"],
                    ifelse(is.infinite(vMod), 1,
                           ifelse(vMod == 0, 0,
                                  (log(vMod, logBase) %% 1)^(1/lambda))))

  vAng    <- ifelse(is.nan(pCompArr$value), hsvNaN["v"],
                    (((argmt - argOffset)/ (2 * pi / pi2Div)) %% 1)^(1/lambda))

  v       <- ifelse(is.nan(pCompArr$value), hsvNaN["v"],
                    darkestShade + (1 - darkestShade) *
                      (gamma * vMod * vAng  +
                         (1 - gamma) * (1 - (1 - vMod) * (1 - vAng))))

  s       <- ifelse(is.nan(pCompArr$value), hsvNaN["s"], stdSaturation)

  pHsvCol$value <- array(hsv(h = h, v = v, s = s), dims)

  return(pHsvCol)

} # phaseModAngColBw

# -----------------------------------------------------------------------------

complexArrayPlotBw <- function(zMetaInfrm,
                               xlim,
                               ylim,
                               pType = "pma",
                               invertFlip = FALSE,
                               pi2Div = 9,
                               logBase = exp(2*pi/pi2Div),
                               argOffset = 0,
                               bwCols = c("black", "white"),
                               hsvNaN = c(0, 0, 0.5),
                               asp = 1,
                               xlab = "", ylab = "",
                               verbose,
                               ...) {

  # Set up plot
  plot(NULL, xlim = xlim, ylim = ylim, asp = asp, xlab = xlab, ylab = ylab, ...)

  # Define call to color transformation function depending user's
  # choice of pType
  colCmd <- switch(pType,
                   "p"   = "phaseColhsv(pListCompArr[[i]],
                                        pHsvCol,
                                        stdSaturation = stdSaturation,
                                        hsvNaN = hsvNaN)",

                   "pm"  = "phaseModColhsv(pListCompArr[[i]],
                                           pHsvCol,
                                           lambda = lambda,
                                           logBase = logBase,
                                           stdSaturation = stdSaturation,
                                           darkestShade = darkestShade,
                                           hsvNaN = hsvNaN)",

                   "pa"  = "phaseAngColhsv(pListCompArr[[i]],
                                           pHsvCol,
                                           lambda = lambda,
                                           pi2Div = pi2Div,
                                           argOffset = argOffset,
                                           stdSaturation = stdSaturation,
                                           darkestShade = darkestShade,
                                           hsvNaN = hsvNaN)",

                   "pma" = "phaseModAngColBw(pListCompArr[[i]],
                                             pHsvCol,
                                             pi2Div = pi2Div,
                                             logBase = logBase,
                                             argOffset = argOffset,
                                             bwCols = bwCols,
                                             darkestShade = darkestShade,
                                             hsvNaN = hsvNaN)"
  ) # switch


  # Obtain the names of the files to load and process
  zMetaInfrm$metaZ$wFileNames <- paste(zMetaInfrm$tempDir,
                                       zMetaInfrm$metaZ$wFileNames, sep = "/")

  # Run the color transformation function over each file
  pHsvCol <- lapply(c(1:nrow(zMetaInfrm$metaZ)),
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
                      pHsvCol <- foreach(i = c(1:length(pListCompArr)),
                                         .export  = c("phaseColhsv",
                                                      "phaseModColhsv",
                                                      "phaseAngColhsv",
                                                      "phaseModAngColhsv",
                                                      "logBase",
                                                      "lambda",
                                                      "gamma",
                                                      "pi2Div",
                                                      "stdSaturation",
                                                      "darkestShade",
                                                      "hsvNaN",
                                                      "newPointer",
                                                      "argOffset"),
                                         .combine = c) %dopar% {

                                           pHsvCol  <- newPointer(NULL)
                                           eval(parse(text = colCmd))      # Does not require a return value,
                                           # changes color array via pointer
                                           pListCompArr[[i]]$value <- NULL # Reduced here, but removed after
                                           # the foreach loop
                                           return(pHsvCol)
                                         } # foreach
                      if(verbose) cat("done.")

                      # Remove the original list of array pointers
                      rm(pListCompArr)

                      # Combine the color arrays in the value of the first pointer.
                      # Free the others (rbindArraysbyPointer).
                      #   Enforce (one-element-) list in case there is only one value
                      #   (i.e. if foreach loop was executed sequentially, one core only)
                      if(length(pHsvCol) == 1) pHsvCol <- list(pHsvCol)
                      pHsvCol <- rbindArraysbyPointer(pHsvCol)

                      return(pHsvCol)
                    }, # function in lapply
                    zMetaInfrm = zMetaInfrm, colCmd = colCmd
  ) # lapply

  # Now combine all blocks into the big raster ...
  if(verbose) cat("\nCombine color rasters ... ")
  pHsvCol <- rbindArraysbyPointer(pHsvCol)
  if(verbose) cat("done.\n")

  # ... and plot it
  if(verbose) cat("Plotting raster image ... ")
  rasterImage(as.raster(pHsvCol$value), xlim[1], ylim[1], xlim[2], ylim[2])
  if(verbose) cat("done.\n")

  pHsvCol$value <- NULL
  rm(pHsvCol)

  return(NULL)

} # complexArrayPlotBw

# -----------------------------------------------------------------------------


phasePortraitBw <- function(FUN, moreArgs = NULL, xlim, ylim,
                            invertFlip = FALSE,
                            res = 150,
                            blockSizePx = 2250000,
                            tempDir = NULL,
                            nCores = parallel::detectCores(),
                            pType = "pma",
                            pi2Div = 9,
                            logBase = exp(2*pi/pi2Div),
                            argOffset = 0,
                            bwCols = c("black", "white"),
                            hsvNaN = c(0, 0, 0.5),
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
                                        function(i, zMetaInfrm, compFun,
                                                 moreArgs) {

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
    complexArrayPlot(zMetaInfrm, xlim, ylim, pType, invertFlip,
                     lambda, gamma, pi2Div, logBase,
                     argOffset, stdSaturation, darkestShade, hsvNaN,
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



