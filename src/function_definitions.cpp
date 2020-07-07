#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
std::complex<double> mandel_1(std::complex<double> z,
                              int itDepth = 52) {
  
  std::complex<double> c = z, zz = 0;
  for(int i = 0; i < itDepth; ++i) {
    zz = pow(zz, 2) + c;
  }
  return zz;  
}


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


// [[Rcpp::export]]
std::complex<double> mandel_3(std::complex<double> z,
                              int itDepth = 52) {
  
  std::complex<double> zz = 0;
  for(int i = 0; i < itDepth && std::abs(z) < 2; ++i) {
    zz = pow(zz, 2) + z;
  }
  if(std::abs(z) >= 2) zz = NAN;
  return zz;  
}





