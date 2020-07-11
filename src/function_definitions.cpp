#include <Rcpp.h>
using namespace Rcpp;


//' @export
// [[Rcpp::export]]
std::complex<double> mandel_1(std::complex<double> z,
                              int itDepth = 52) {

  std::complex<double> c = z, zz = 0;
  for(int i = 0; i < itDepth; ++i) {
    zz = pow(zz, 2) + c;
  }
  return zz;
}



//' @export
// [[Rcpp::export]]
std::complex<double> mandel_2(std::complex<double> z,
                              int itDepth = 52) {

  std::complex<double> zz = 0;
  for(int i = 0; i < itDepth; ++i) {
    zz = pow(zz, 2) + z;
    if(std::abs(zz) >= 2) {
      zz = NAN;
      break;
    }
  }
  return zz;
}



//' @export
// [[Rcpp::export]]
std::complex<double> mandel_3(std::complex<double> z,
                              int itDepth = 52) {
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


// This is the fastest variant, especially with high iteration dephts

//' @export
// [[Rcpp::export]]
std::complex<double> mandel_4(std::complex<double> z,
                              int itDepth = 52) {

  std::complex<double> zz      = 0;
  std::complex<double> zz_prev = 0;

  for(int i = 0; i < itDepth; ++i) {
    zz_prev = zz;
    zz      = pow(zz, 2) + z;
    if(zz == zz_prev) {
      break;
    }
    if((pow(real(zz), 2) + pow(imag(zz), 2)) >= 4) {
      zz = NAN;
      break;
    }
  }
  return zz;
}



