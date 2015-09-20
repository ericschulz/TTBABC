//C++-function to split up a matrix into a learning and a test set
#include <Rcpp.h>
#include <math.h>  
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::export]]
List mysplit(NumericMatrix Data, double proportion) {
  int ncold = Data.ncol(); 
  int nrowd = Data.nrow();
  int ns = round(proportion*nrowd);
  NumericVector storvec(ns); //array to store the random numbers in
  NumericMatrix X_learn(ns, ncold);
  NumericMatrix X_test(nrowd-ns, ncold);
  int nrowl = X_learn.nrow();
  int MAX = nrowd;
  srand(time(NULL)); //always seed your RNG before using it
  //generate random numbers:
  for (int i=0;i<ns;i++)
  {
    bool check; //variable to check or number is already used
    int n; //variable to store the number in
    do
    {
      n=rand()%MAX;
      //check or number is already used:
      check=true;
      for (int j=0;j<i;j++)
        if (n == storvec[j]) //if number is already used
        {
          check=false; //set check to false
          break; //no need to check the other elements of value[]
        }
    } while (!check); //loop until new, unique number is found
    storvec[i]=n; //store the generated number in the array
    X_learn(i,_)=Data(n,_);
  }
  int track=0;
  for (int k=0;k<nrowd;k++)
    {
    int item=k;
    if (std::find(storvec.begin(), storvec.end(), item)==storvec.end()){
      X_test(track,_)=Data(k,_);
      track++;
      }
    }
  return Rcpp::List::create(Rcpp::Named( "learn" ) = X_learn,
                            Rcpp::Named( "test" ) = X_test); 
}