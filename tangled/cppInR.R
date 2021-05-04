library(Rcpp)

Rcpp::sourceCpp('cppInR.cpp')

Rcpp::cppFunction('NumericVector cppIntCat(NumericVector a, NumericVector b) {
  Rcpp::NumericVector c(a.size() + b.size());
  int j=0;
  for (int i=0; i<a.size(); i++)
    c[j++] += a[i];
  for (int i=0; i<b.size(); i++)
    c[j++] += b[i];
  return c;
}')

Rcpp::cppFunction('double cppIntSum(NumericVector x) {
  double total = 0;
  for(int i=0; i<x.size(); ++i) {
    total += x[i];
  }
  return total;
}')

cppIntCat(1:10, 2:11)

cppExtCat(1:10, 2:11)

cppIntSum(1:10)

cppExtSum(1:10)
