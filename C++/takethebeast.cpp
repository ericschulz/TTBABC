#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

DataFrame  takethebeast(NumericMatrix validities, NumericVector weights, NumericMatrix x, NumericVector y) {
  int nrow = x.nrow(); 
  int ncol = x.ncol();
  int validitiesnrow=validities.nrow();
  NumericVector checkvector(validitiesnrow); //array to store the random numbers in
  
  for (int h=0;h<validitiesnrow;h++){
    NumericVector prediction(nrow); //array to store the random numbers in
    
    for (int i=0;i<nrow;i++)
      {
    
      for (int j=0;j<ncol;j++){
        if(prediction[i]==0){
          prediction[i]=x(i,validities(h,j))*weights(j);
          }
    
        }
    
      }
    
    for (int k=0;k<nrow;k++)
    {if (abs(prediction[k]-y[k])<0.01)
        checkvector[h]++;
        }
    }
  
  return Rcpp::DataFrame::create(Named("checks") = checkvector); 
}