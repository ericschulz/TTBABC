#This is a script to estimate the underlying ground truth of cue orders in the city data set bu brute force
#Copyright@Schulz, Speekenbrink & Meder
#The estimated performance will be based on the assumption that weights are always positive

#Housekeeping
rm(list=ls())

#Function to create all possible permutations of numbers up to n
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

#Creating all possible points within a matrix
for (l in 1:9){
  m<-data.frame(matrix((c(1:l,rep(0,9-l+1))[permutations(9)]),ncol=9))
  if (l==1){
    m1<-m[!duplicated(paste0(m[,1:9])),]
  } else{
    #binding it together
      m1<-rbind(m1, m[!duplicated(paste0(m[,1:9])),])
    }

}

#change it to a data frame
m1<-data.frame(m1)
#deleting duplicats
m1<-m1[!duplicated(m1), ]
#garbagwe control
rm(m)
#read in city compare data
dat<-read.csv(file.choose())
#get rid of X
dat$X<-NULL
#9 cases in total
cases<-9
#performance vector
performance<-rep(0, nrow(m1))
#loop over matrix
#LONG RUN TIME STARTS HERE
for (i in 1:nrow(m1)){
  #progress bar
  pb <- txtProgressBar(min=1, max=nrow(m1), style=3)
  #order frame
  ttborder<-data.frame(n=1:9, dir=as.numeric(m1[i,]))
  #trick to order from 1-9
  ttborder$dir<-ifelse(ttborder$dir!=0, ttborder$dir^-1, 0)
  #order in dependency of validity
  ttborder<-ttborder[order(abs(ttborder$dir), decreasing=TRUE),]
  #get the weights, 0 (unimportant) or 1 (important and pre-ordered)
  ttborder$dir<-ifelse(ttborder$dir>0,1,0)
  ##Predict TTB
  ###copy dt to avoid that the changed order messes things up later
  dtcopy<-dat
  ###pre-order by weight magnitude
  dtcopy[,1:cases]<-dtcopy[,ttborder$n]
  ###loop through order
  for (j in 1:nrow(ttborder)){
    ####Output=win or loss_j times weight
    dtcopy[,j]<-dtcopy[,j]*ttborder$dir[j]
    }
  ###Generate predictions by looping through ordered copy of test set
  predttb<-rep(0,nrow(dtcopy))
  ###loop through
  for (j in 1:nrow(ttborder)){
    ####prediction is the ordered copy times the weights
    predttb<-ifelse(predttb==0, dtcopy[,j], predttb)
    }
  ###if 0, then guess
  predttb<-ifelse(predttb==0, sample(c(-1,1),1),predttb)
  #collect mean performance
  performance[i]<-mean(predttb==dat$y)
  #set progress bar
  setTxtProgressBar(pb, i)
}
#Collect
m1$performance<-performance
#write data file
write.csv(m1, "bruteforcecity.csv")
#END
performance<-NULL
plot(m1$performance)
dev.off()

