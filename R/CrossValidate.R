#Cross-Validation of Different TTB-variants
#Copyright@Schulz, Meder & Speekenbrink


#House keeping
rm(list=ls())

#set wd
setwd("/home/hanshalbe/Desktop/TTBABC")

#read in data
data<-read.csv("data/city.csv")
data$X<-NULL

#Load packages
packages <- c('ggplot2', 'plyr', 'Rcpp', 'matrixStats')
lapply(packages, library, character.only = TRUE)

#Source files for R
source("R/permutations.R")
source("R/ttbabcfit.R")    
source("R/ttbabcpredict.R")    

#Source files for Rcpp
sourceCpp("C++/pairwise.cpp")
sourceCpp("C++/takethebeast.cpp")
sourceCpp("C++/ttb.cpp")
sourceCpp("C++/mysplit.cpp")

#Establish ground truth
##All pairwise comparisons
allcomps<-pairwise(as.matrix(data))
#mark y
marky<-ncol(data)
#mark columns of X
endx<-marky-1
#threshold, win=1, loss=-1
allcomps[,marky]<-ifelse(allcomps[,marky]>0,1,-1)
#X is all independent variables
X<-allcomps[,1:endx]
#y is the dependent variable
y<-allcomps[,marky]
#find best cue order by brute force
allperms<-matrix((1:endx)[permutations(endx)], ncol=endx)
#weights have to be give
weights<-sign(cor(X,y))
#take the beast finds best cue order by brute force
m<-takethebeast(validities = allperms, weights=weights[,1], x=as.matrix(X), y=y)  
#remeber the best order
bestorder<-allperms[which.max(m$checks),]
#get rid of big vectors and matrices
rm(m,allperms)
#calculate best score by discounted cumulative gain
bestscore<-9:1/log2(1:9)
bestscore<-ifelse(bestscore==Inf,9,bestscore)
#get sume
best<-sum(bestscore)
#times of comparisons here fixed to be 30
times<-30
#proportions of learning set
props<-seq(0.1,0.9,0.1)
#final storage matrix
dfinal<-matrix(NA,nrow=9, ncol=12)
#loop over proportions
for (h in seq_along(props)){
  
  #dummy frame
  dout<-data.frame(abcmu=rep(0,times), abcscore=rep(0,times),
                   ttbmu=rep(0,times), ttbscore=rep(0,times),
                   ttnmu=rep(0,times), ttnscore=rep(0,times))
  #check times number of times
  for (i in 1:times){
    
    #split up frame into learning and test frame
    splitted<-mysplit(as.matrix(data), proportion = props[h])
    #learn set
    learnset<-pairwise(as.matrix(splitted$learn))
    #test set
    testset<-pairwise(as.matrix(splitted$test))
    #X_learn: independent variables of learning set
    X_learn<-learnset[,-marky]
    #X_test: independent variables of learning set
    X_test<-testset[,-marky]
    #y_learn: dependent of learning set
    y_learn<-sign(learnset[,marky]-0.01)
    #y_test dependent of test set
    y_test<-sign(testset[,marky]-0.01)
    
    
    #TTB_ABC
    ##Fit model
    mabc<-ttbabcfit(X=X_learn, y=y_learn, epsilon=0.75, proportion=0.02)
    #get mean predictive performance
    dout$abcmu[i]<-mean(ttbabcpredict(mabc, testset[,-ncol(testset)])$class==y_test)
    #get importance order
    abcimp<-order(mabc$m$ballcounts)
    #match orders
    abcorder<-rep(0,length(abcimp))
    for (k in seq_along(bestorder)){ 
      abcorder[which(abcimp==bestorder[k])]<-marky-k
    }
    #get score
    abcscore<-abcorder/log2(1:endx)
    abcscore<-ifelse(abcscore==Inf,abcorder,abcscore)
    #standardise
    dout$abcscore[i]<-sum(abcscore)/best
   
     #Classic TTB
    ##positive validity
    positive<-apply(X_learn,2, function(x){sum(x==y_learn)/sum(x!=0)})
    positive<-ifelse(is.nan(positive), 0, positive)
    #negative validty
    negative<-apply(X_learn,2, function(x){sum(x==-y_learn)/sum(x!=0)})
    negative<-ifelse(is.nan(negative), 0, negative)
    #classic order
    classicorder<-matrix(rank(1-rowMaxs(cbind(positive, negative))), nrow=1)
    #predictions
    preds<-sign(ttb(validities = classicorder, weights = sign(positive-0.50001), x = as.matrix(X_test)))
    #store predictive performance
    dout$ttbmu[i]<-mean(preds$class==y_test)
    #get importance
    ttbimp<-order(classicorder)
    #match with true order
    ttborder<-rep(0,length(ttbimp))
    for (k in seq_along(ttborder)){ 
      ttborder[which(ttbimp==bestorder[k])]<-marky-k
    }
    #calculate discounted cumulative gain
    ttbscore<-ttborder/log2(1:endx)
    ttbscore<-ifelse(ttbscore==Inf,ttborder,ttbscore)
    #store in frame
    dout$ttbscore[i]<-sum(ttbscore)/best
    
    #Updated TTB (Newell et. al, 2004)
    ##validty
    validity<-apply(cbind(positive, negative),1, max)
    #discrimination rate
    discrimination<-apply(X_learn,2, function(x){sum(x!=0)})/nrow(X_learn)
    #updated cue validity, corrected for discrimination rate
    updated<-validity*discrimination+0.5*(1-discrimination)
    #updated order
    updatedorder<-matrix(rank(1-updated), nrow=1)
    #get predictions
    preds<-sign(ttb(validities = updatedorder, weights = sign(positive-0.50001), x = as.matrix(X_test)))
    #check predictive performance
    dout$ttnmu[i]<-mean(preds$class==y_test)
    #get importance
    ttnimp<-order(updatedorder)
    #match with true order
    ttnorder<-rep(0,length(ttnimp))
    for (k in seq_along(ttnorder)){ 
      ttnorder[which(ttnimp==bestorder[k])]<-marky-k
    }
    #calculate discounted cumulative gain
    ttnscore<-ttnorder/log2(1:endx)
    ttnscore<-ifelse(ttnscore==Inf,ttnorder,ttnscore)
    #stanardise and store
    dout$ttnscore[i]<-sum(ttnscore)/best
  }
  #get mean performance per model
  dfinal[h,1:6]<-apply(dout,2,mean)
  #get standard error of mean performance per model
  dfinal[h,7:12]<-apply(dout,2, function(x){sd(x)/sqrt(length(x))})
  #save on the go
write.csv(dfinal, "data/citycross.csv")
}
#END