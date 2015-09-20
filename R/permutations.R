#permutations:

##Takes in a number n and returns all possible permutations
##Example: n=2 => (1,2)(2,1)

permutations <- function(n){
  #if 1, then unnecessary
  if(n==1){
    return(matrix(1))
  } else {
    sp<-permutations(n-1)
    p <-nrow(sp)
    A <-matrix(nrow=n*p,ncol=n)
    for(i in 1:n){
      A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
    }
    return(A)
  }
}
#END FUNCTION