#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
DataFrame  pairwise(NumericMatrix data) {
  int nrow=data.nrow();
  int ncol=data.ncol();
  int size=((nrow-1)*nrow)/2;
  NumericMatrix out(size,ncol);
  int counter=0;
  
  for (int i=0;i<nrow-1;i++){
    for (int j=i+1;j<nrow;j++){
      for (int k=0;k<ncol;k++){
        out(counter,k)=data(i,k)-data(j,k);
        }
      counter++;
      }
  }
  
return Rcpp::DataFrame::create(Named("comparisons") = out); 
}