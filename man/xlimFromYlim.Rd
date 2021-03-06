% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Helpers.R
\name{xlimFromYlim}
\alias{xlimFromYlim}
\title{Adjust xlim to ylim}
\usage{
xlimFromYlim(ylim, centerX = 0, x_to_y = 16/9)
}
\arguments{
\item{ylim}{Numeric vector of length 2; the fixed lower and upper boundary
of the vertical coordinate range}

\item{centerX}{The horizontal coordinate which the output range is to be
centered around (default = 0)}

\item{x_to_y}{The desired ratio of the horizontal (x) to the vertical (y)
range. Default is 16/9, a display ratio frequently used for computer or
mobile screens}
}
\value{
A numeric vector of length 2; the lower and upper boundary of the
  resulting vertical coordinate range
}
\description{
This simple function is useful for adjusting x and y coordinate ranges
\code{xlim} and \code{ylim} in order to maintain a desired display ratio. The
latter must be given, the former will be adjusted.
}
\details{
For certain purposes, e.g. producing a graph that exactly matches a screen,
the x and y coordinates must be adjusted to match a given display ratio. If
the vertical range, \code{ylim}, the desired ratio, \code{x_to_y} and the
desired center of the x-range, \code{centerX}, are provided, this function
returns an adpated vertical range, that can be used as \code{ylim} in any
plot including \code{\link{phasePortrait}}.
}
\examples{
# Make a phase portrait of a pretty function that fully covers a
# plot with a display aspect ratio of 5/4.

# 9 inch wide window with 5/4 display ratio (x/y)
\donttest{
# x11(width = 9, height = 9 * 4/5) # Screen device commented out
                                   # due to CRAN test requirements.
                                   # Use it when trying this example
ylim <- c(-8, 7)
xlim <- xlimFromYlim(ylim, centerX = 0, x_to_y = 5/4)
op <- par(mar = c(0, 0, 0, 0), bg = "black") # Omit all plot margins
phasePortrait("exp(cosh(1/(z - 2i + 2)^2 * (1/2i - 1/4 + z)^3))", pType = "pm",
xlim = xlim, ylim = ylim, # Apply the coordinate ranges
xaxs = "i", yaxs = "i",   # Allow for now room between plot and axes
nCores = 2) # Max. two cores allowed on CRAN
            # not a limit for your own use
par(op)
  \dontshow{
  # R CMD check: make sure any open connections are closed afterward
  foreach::registerDoSEQ()
  doParallel::stopImplicitCluster()
  }
}


}
\seealso{
Other helpers: 
\code{\link{vector2String}()},
\code{\link{ylimFromXlim}()}
}
\concept{helpers}
