#include <Rcpp.h>
using namespace Rcpp;
//' @title A Gibbs sampler using Rcpp
//' @name Gibbs 
 //' @description A Gibbs sampler using Rcpp
 //' @param N the number of samples
 //' @param burn the number of burn-in sample
 //'@param a,b,n parameters of the distribution
 //' @return a random sample of size \code{N}
 //' @import Rcpp
 //' @importFrom Rcpp evalCpp
//' @importFrom stats rbinom rbeta
 //' @examples
 //' \dontrun{
 //'gibbsSampler(5000, 1000, 1, 1, 10) 
 //' }
 //' @export
// [[Rcpp::export]]

NumericMatrix gibbsSampler(int N, int burn, int a, int b, int n) {
  NumericMatrix X(N, 2);
  
  int x = 0;
  double y = 0.5;
  
  for (int i = 0; i < N; ++i) {
    x = R::rbinom(1, y * n);
    y = R::rbeta(x + a, (n - x) + b);
    
    X(i, 0) = x;
    X(i, 1) = y;
  }
  
  NumericMatrix result = X(Range(burn, N - 1), _);
  return result;
}

