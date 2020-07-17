#include <Rcpp.h>
using namespace Rcpp;




//' @export
// [[Rcpp::export]]
std::complex<double> mandelbrot(std::complex<double> z,
                              int itDepth = 500) {
  std::complex<double> zz = 0;
  for(int i = 0; i < itDepth; ++i) {
    zz = pow(zz, 2) + z;
    if((pow(real(zz), 2) + pow(imag(zz), 2)) >= 4) {
      zz = NAN;
      break;
    }
  }
  return zz;
}



// Normal Julia set fc(z) = z² + c

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
      zz = NAN;
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
  std::complex<double> one = 1;

  for(int i = 0; i < n; ++i) {
    zz = zz * std::abs(a[i]) / a[i]
            * (a[i] - z) / (one - z * std::conj(a[i]));
  }
  return zz;
}






