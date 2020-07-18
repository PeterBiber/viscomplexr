#include <Rcpp.h>
using namespace Rcpp;


//' Mandelbrot iteration with a given number of steps
//'
//' This function is provided as a basis for visualizing the Mandelbrot set with
//' \code{\link{phasePortrait}}. While usual visualisations color the points
//' \emph{outside} the Mandelbrot set dependent on the velocity of divergence,
//' this function produces the information required for coloring the Mandelbrot
//' set itself. For numbers that can be identified as not being elements of the
//' Mandelbrot set set obtain a \code{NaN+NaNi} value; for all other numbers,
//' the function gives back the value after a user-defined number of iterations.
//' The function has been implemented in C++; it runs fairly fast.
//'
//' The Mandelbrot set comprises all complex numbers \code{z} for which the
//' sequence \code{a[n+1] = a[n]^2 + z} starting with \code{a[0] = 0} remains
//' bounded for all \code{n > 0}. This condition is certainly not true, if, at
//' any time, \code{abs(a[]) >= 2}. The function \code{mandelbrot} performs the
//' iteration for \code{n = 0, ..., itDepth - 1} and permanently checks for
//' \code{abs(a[n+1]) >= 2}. If this is the case, it stops the iteration and
//' returns \code{NaN+NaNi}. In all other cases, it returns \code{a[itDepth]}.
//'
//' @param z Complex number; the point in the complex plane to which the output
//'   of the function is mapped
//'
//' @param itDepth An integer which defines the depth of the iteration, i.e. the
//'   maximum number of iteration.
//'
//' @return Either \code{NaN+NaNi} or the complex number obtained after
//'   \code{itDepth} iterations
//'
//' @family fractals
//'
//' @examples
//' \dontrun{
//' # This code shows the famous Mandelbrot figure in total, just in the
//' # opposite way as usual: the Mandelbrot set itself is colored, while the
//' # points outside are uniformly black.
//' # Adjust xlim and ylim to zoom in whereever you like.
//'
//' phasePortrait(mandelbrot,
//'   xlim = c(-2.3, 0.7),
//'   ylim = c(-1.2, 1.2),
//'   hsvNaN = c(0, 0, 0)
//' )
//' }
//'
//' @export
// [[Rcpp::export]]
std::complex<double> mandelbrot(std::complex<double> z,
                                int itDepth = 500) {
  std::complex<double> zz = 0;
  for(int i = 0; i < itDepth; ++i) {
    zz = pow(zz, 2) + z;
    if((pow(real(zz), 2) + pow(imag(zz), 2)) >= 4) {
      zz = std::complex<double>(NAN, NAN);
      break;
    }
  }
  return zz;
}



// Normal Julia set fc(z) = z² + c


//' Julia iteration with a given number of steps
//'
//' @family fractals
//'
//' @export
// [[Rcpp::export]]
std::complex<double> juliaNormal(std::complex<double> z,
                                 std::complex<double> c,
                                 double R_esc,
                                 int itDepth = 500) {

  // squared escape radius
  // double Rq     = pow(1/2 * (1 + sqrt(1 + 4 * std::abs(c))), 2);
  double Rq     = pow(R_esc, 2);

  std::complex<double> zz = z;
  for(int i = 0; i < itDepth; ++i) {
    zz = pow(zz, 2) + c;
    if((pow(real(zz), 2) + pow(imag(zz), 2)) >= Rq) {
      zz = std::complex<double>(NAN, NAN);
      break;
    }
  }
  return zz;
}



//' @export
// [[Rcpp::export]]
std::complex<double> blaschkeProd(std::complex<double> z,
                                  std::vector<std::complex<double>> a) {
  int n = a.size();
  std::complex<double> zz  = 1;
  // std::complex<double> one = 1;

  for(int i = 0; i < n; ++i) {
    zz = zz * std::abs(a[i]) / a[i]
            * (a[i] - z) / (std::complex<double>(1, 0) - z * std::conj(a[i]));
  }
  return zz;
}






