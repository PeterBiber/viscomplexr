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
//' @family maths
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
//' @family maths
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


//' Calculate Blaschke products
//'
//' This function calculates Blaschke products
//' (\url{https://en.wikipedia.org/wiki/Blaschke_product}) for a complex number
//' \code{z} given a sequence \code{a} of complex numbers inside the unit disk,
//' which are the zeroes of the Blaschke product.
//'
//' A sequence of points \code{a[n]} located inside the unit disk satisfies the
//' Blaschke condition, if \code{sum[1:n] (1 - abs(a[n])) < Inf}. For each
//' element \code{a != 0} of such a sequence, \code{B(a, z) = abs(a)/a * (a -
//' z)/(1 - conj(a) * z)} can be calculated. For \code{a = 0}, \code{B(a, z) =
//' z}. The Blaschke produkt \code{B(z)} results as \code{B(z) = prod[1:n]
//' (B(a[n], z))}.
//'
//' @param z Complex number; the point in the complex plane to which the output
//'   of the function is mapped
//'
//' @param a Vector of complex numbers located inside the unit disk. At each
//'   \code{a}, the Blaschke product will have a zero.
//'
//' @return The value of the Blaschke product at \code{z}.
//'
//' @family maths
//'
//' @examples
//' \dontrun{
//' # Generate random vector of 17 zeroes inside the unit disk
//' n <- 17
//' a <- complex(modulus = runif(n, 0, 1), argument = runif(n, 0, 2*pi))
//'
//' # Portrait the Blaschke product
//' phasePortrait(blaschkeProd, moreArgs = list(a = a),
//'   xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))
//' }
//'
//' @export
// [[Rcpp::export]]
std::complex<double> blaschkeProd(std::complex<double> z,
                                  std::vector<std::complex<double>> a) {
  int n = a.size();
  std::complex<double> zz = 1;
  std::complex<double> fact;

  for(int i = 0; i < n; ++i) {
    if(std::abs(a[i]) != 0) {
      fact = std::abs(a[i]) / a[i] * (a[i] - z) /
        (std::complex<double>(1, 0) - z * std::conj(a[i]));
    }
    else {
      fact = z;
    }
    zz = zz * fact;
  }
  return zz;
}



//' @export
// [[Rcpp::export]]
std::complex<double> jacobiTheta(std::complex<double> z,
                                 std::complex<double> tau,
                                 int kIter = 30) {

  std::complex<double> q = exp(std::complex<double>(0, 1) * M_PI * tau);

  std::complex<double> theta = std::complex<double>(1, 0);
  int k = 0;
  for(int i = 0; i < kIter; i++) {
    k++;
    std::complex<double> g = exp(2 * M_PI * std::complex<double>(0, 1) * z);
    theta += pow(q, pow(k, 2)) * pow(g, k) + pow(q, pow(k, 2)) * pow(g, -k);
  }
  return theta;
}








