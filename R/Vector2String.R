
#' Convert a vector into a comma-separated string
#'
#' This is a utility function that transforms any vector into a single character
#' string, where the former vector elements are separated by commas. This is
#' useful for feeding a series of constant numeric values to
#' \code{\link{phasePortrait}}.
#'
#' @param vec The (usually real or complex valued) vector to be converted.
#'
#' @return A string, where the former vector elements are separated by commas,
#'   enclosed between "c(" and ")".
#'
#' @export
#'
#' @examples
#' # Make a vector of 77 complex random numbers inside the unit circle
#' n <- 77
#' a <- complex(n, modulus = runif(n), argument = 2*pi*runif(n))
#' a <- vector2String(a)
#' print(a)
#'
#'
#' # Use this for portraiting a Blasche product
#' \dontrun{
#' x11(width = 9.45, height = 6.30)
#' op <- par(mar = c(1, 1, 1, 1), bg = "black")
#' n <- 77
#' a <- complex(n, modulus = runif(n), argument = 2*pi*runif(n))
#' a <- vector2String(a)
#' FUN <- paste("vapply(z, function(z, a){
#'                     return(prod(abs(a)/a * (a-z)/(1-Conj(a)*z)))
#'                    }, a =", a,
#'              ", FUN.VALUE = complex(1))", sep = "")
#' phasePortrait(FUN, pType = "p", axes = FALSE,
#'               xlim = c(-3, 3), ylim = c(-2.0, 2.0))
#' par(op)}
#'
#'
vector2String <- function(vec) {

  n    <- length(vec)
  rVec <- paste(vec, collapse = ", ")
  rVec <- paste("c(", rVec, ")", sep = "")
  return(rVec)

}
