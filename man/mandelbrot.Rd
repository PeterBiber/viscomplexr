% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{mandelbrot}
\alias{mandelbrot}
\title{Mandelbrot iteration with a given number of steps}
\usage{
mandelbrot(z, itDepth = 500L)
}
\arguments{
\item{z}{Complex number; the point in the complex plane to which the output
of the function is mapped}

\item{itDepth}{An integer which defines the depth of the iteration, i.e. the
maximum number of iteration (default: \code{itDepth =  500})}
}
\value{
Either \code{NaN+NaNi} or the complex number obtained after
  \code{itDepth} iterations
}
\description{
This function is provided as a basis for visualizing the Mandelbrot set with
\code{\link{phasePortrait}}. While usual visualizations color the points
\emph{outside} the Mandelbrot set dependent on the velocity of divergence,
this function produces the information required for coloring the Mandelbrot
set itself. For numbers that can be identified as not being elements of the
Mandelbrot set, we obtain a \code{NaN+NaNi} value; for all other numbers,
the function gives back the value after a user-defined number of iterations.
The function has been implemented in C++; it runs fairly fast.
}
\details{
The Mandelbrot set comprises all complex numbers \code{z} for which the
sequence \code{a[n+1] = a[n]^2 + z} starting with \code{a[0] = 0} remains
bounded for all \code{n > 0}. This condition is certainly not true, if, at
any time, \code{abs(a[]) >= 2}. The function \code{mandelbrot} performs the
iteration for \code{n = 0, ..., itDepth - 1} and permanently checks for
\code{abs(a[n+1]) >= 2}. If this is the case, it stops the iteration and
returns \code{NaN+NaNi}. In all other cases, it returns \code{a[itDepth]}.
}
\examples{
# This code shows the famous Mandelbrot figure in total, just in the
# opposite way as usual: the Mandelbrot set itself is colored, while the
# points outside are uniformly black.
# Adjust xlim and ylim to zoom in wherever you like.
\donttest{
phasePortrait(mandelbrot,
  xlim = c(-2.3, 0.7),
  ylim = c(-1.2, 1.2),
  hsvNaN = c(0, 0, 0),
  nCores = 1)          # Max. two cores on CRAN, not a limit for your use
  \dontshow{
  # R CMD check: make sure any open connections are closed afterward
  foreach::registerDoSEQ()
  doParallel::stopImplicitCluster()
  }
}

}
\seealso{
Other fractals: 
\code{\link{juliaNormal}()}

Other maths: 
\code{\link{blaschkeProd}()},
\code{\link{jacobiTheta}()},
\code{\link{juliaNormal}()}
}
\concept{fractals}
\concept{maths}
