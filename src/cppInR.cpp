///////////////////////////////////////////////////////////
#include <Rcpp.h>

///////////////////////////////////////////////////////////
using namespace Rcpp;

///////////////////////////////////////////////////////////
// [[Rcpp::export]]
NumericVector cppExtCat(NumericVector a, NumericVector b) {
  Rcpp::NumericVector c(a.size() + b.size());
  int j=0;
  for (int i=0; i<a.size(); i++)
    c[j++] += a[i];
  for (int i=0; i<b.size(); i++)
    c[j++] += b[i];
  return c;
}

///////////////////////////////////////////////////////////
// [[Rcpp::export]]
double cppExtSum(NumericVector x) {
  double total = 0;
  for(int i=0; i<x.size(); ++i) {
    total += x[i];
  }
  return total;
}

///////////////////////////////////////////////////////////
/*** R
print('HI -- I am some code in cppRcppOutOfLine.cpp')
*/
