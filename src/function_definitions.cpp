#include <Rcpp.h>
using namespace Rcpp;


//' Mandelbrot iteration with a given number of steps
//'
//' This function is provided as a basis for visualizing the Mandelbrot set with
//' \code{\link{phasePortrait}}. While usual visualisations color the points
//' \emph{outside} the Mandelbrot set dependent on the velocity of divergence,
//' this function produces the information required for coloring the Mandelbrot
//' set itself. For numbers that can be identified as not being elements of the
//' Mandelbrot set, we obtain a \code{NaN+NaNi} value; for all other numbers,
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
//'   maximum number of iteration (default: \code{itDepth =  500})
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


//' Julia iteration with a given number of steps
//'
//' This function is designed as the basis for visualizing normal Julia sets
//' with \code{\link{phasePortrait}}. In contrast to usual visualisations of
//' Julia sets, this requires coloring the actual member points of the set and
//' not the points outside. Therefore, for numbers that can be identified as not
//' being parts of the Julia set, this function returns \code{NaN+NaNi}. All
//' other numbers are mapped to the complex value obtained after a user-defined
//' number of iterations. This function has been implemented in C++; therefore
//' it is fairly fast.
//'
//' Normal Julia sets are closely related to the Mandelbrot set. A normal Julia
//' set comprises all complex numbers \code{z} for which the following sequence
//' is bounded for all \code{n > 0}: \code{a[n+1] = a[n]^2 + c}, starting with
//' \code{a[0] = z}. The parameter \code{c} is a complex number, and the
//' sequence is certainly unbounded if \code{abs(a[]) >= R} with \code{R} being
//' an escape Radius which matches the inequality \code{R^2 - R >= abs(c)}. As
//' the visulation with this package gives interesting pictures (i.e. other than
//' a blank screen) only for \code{c} which are elements of the Mandelbrot set,
//' \code{R = 2} is a good choice. For the author's taste, the Julia
//' visualisations become most interesting for \code{c} located in the border
//' zone of the Mandelbrot set.
//'
//' @param z Complex number; the point in the complex plane to which the output
//'   of the function is mapped
//'
//' @param c Complex number; a parameter whose choice has an enormous effect on
//'   the shape of the Julia set. For obtaining useful results with
//'   \code{\link{phasePortrait}}, \code{c} must be an element of the Mandelbrot
//'   set.
//'
//' @param R_esc Real number; the espace radius. If the absolute value of a
//'   number obtained during iteration attains or excels the value of
//'   \code{R_esc}, \code{juliaNormal} will return \code{NaN+NaNi}. \code{R_esc
//'   = 2} is a good choice for \code{c} being an element of the Mandelbrot set.
//'   See Details for more information.
//'
//' @param itDepth An integer which defines the depth of the iteration, i.e. the
//'   maximum number of iteration (default: \code{itDepth =  500})
//'
//' @return Either \code{NaN+NaNi} or the complex number obtained after
//'   \code{itDepth} iterations
//'
//' @family fractals
//'
//' @examples
//' \dontrun{
//' # This code visualizes a Julia set with some appeal (for the author's
//' # taste). Zoom in as you like by adjusting xlim and ylim.
//'
//' phasePortrait(juliaNormal,
//'   moreArgs = list(c = -0.09 - 0.649i, R_esc = 2),
//'   xlim = c(-2, 2),
//'   ylim = c(-1.3, 1.3),
//'   hsvNaN = c(0, 0, 0))
//' }
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


//' Calculate Blaschke Products
//'
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






