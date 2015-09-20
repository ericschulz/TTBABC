#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
DataFrame  ttb(NumericVector validities, NumericVector weights, NumericMatrix x) {
  int nrow = x.nrow();
  int ncol = x.ncol();
  NumericVector prediction(nrow);
  
  for (int i=0;i<nrow;i++){
    for (int j=0;j<ncol;j++){
   if(prediction[i]==0){prediction[i]=x(i,validities(j))*weights(j);}
    }
  }
  return Rcpp::DataFrame::create(Named("class") = prediction); 
  
}