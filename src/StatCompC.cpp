#include <Rcpp.h>
using namespace Rcpp;

//' @title Laplace density function
//' @description Standard Laplace density function
//' @param x a real number
//' @return the density at \code{x}
//' @examples
//' @export
// [[Rcpp::export]]
double dlaplace(double x) {
  return 0.5*exp(-abs(x));
}

//' @title Random walk Metropolis sampler
//' @description generate random samples from standard laplace distribution using random walk Metropolis sampler
//' @param sigma the standard deviation for Gaussian increment
//' @param x0 initial point of random walk
//' @param N the numer of wanted samples
//' @return a list containing the samples and the number of rejected points
//' @useDynLib StatComp20026
//' @examples
//' @export
// [[Rcpp::export]]
List rwM(double sigma, double x0, int N){
  NumericVector x (N);
  double y = 0;
  x[0] = x0; 
  NumericVector u = runif(N);
  int k = 0; 
  
  for(int i = 1; i < N; i++) {
    y = rnorm(1, x[i-1], sigma)[0];
    if(u[i]<=(dlaplace(y)/dlaplace(x[i-1]))){
      x[i] = y;
    }
    else{
      x[i] = x[i-1];
      k++;
    }
  } 
  
  return(List::create(x, k));
}

