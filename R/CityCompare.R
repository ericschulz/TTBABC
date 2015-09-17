#This is Example 4 from Schulz, Speekenbrink & Meder
#We are checking out TTB-ABC's predictive performance in the city size data set
#This script demonstrates that TTB-ABC can perform as well as traditional TTB
#The city size data set is the most (over-)used data set in the heuristic literature, see 
#Input for TTB-ABC: 
##4 trinary x variables: Soccer_Team, State_Capital, Former_East_Germany, 
##                       Industrial_Belt, Licence_plate, Intercity_trainline, 
##                       Exposition_Site, National_Capital, University
##1 binary y variable: city size(population size)

#Output:
##Data frame that contains the performance for both TTB and TTB-ABC
#House keeping
rm(list=ls())
setwd("/home/hanshalbe/Desktop/TTBABC")
#Load packages
packages <- c('ggplot2', 'plyr', 'rpart')
lapply(packages, library, character.only = TRUE)

#Load TTB-ABC source
source("R/ttbabcfit.R")
source("R/ttbabcpredict.R")

#function to calculate standard error
standerr<-function(x){
  y<-sd(x)/sqrt(length(x))
  return(y)
}

#function to get differences of pairwise comparisons
mycomfn<-function(x, df){
  outframe<-data.frame(df[x[1],] - df[x[2],])
  return(outframe)
}

#read in city data
city<-read.csv("data/city.csv")

#learning set size ={10%,20%,30%,40%,50%,60%,70%,80%,90%}
setsize<-seq(10,90,10)

#data set to collect mean performance and standard error
dfinal<-data.frame(ttbmu=rep(0,9), ttbse=rep(0,9),              
                   abcmu=rep(0,9), abcse=rep(0,9))

#looping through the set size
for (k in 3:9){
  #initialize the performance collectors
  checkttb<-rep(0,50)
  checkabc<-rep(0,50)
  #looping through the collectors
  for (l in 1:50){
    #sample an index based on the current setsize_k
    index<-sample(1:nrow(city), round(0.01*setsize[k]*nrow(city)))
    #create learning set
    dlearn<-city[index,]
    #create test set
    dtest<-city[-index,]
    #generate all possible pairwise comparisons
    combol <- combn(nrow(dlearn), 2)
    combot <- combn(nrow(dtest), 2)
    #generate data set with all pairwise comparisons from city data
    dl<-adply(combol, 2, function(x){mycomfn(x, dlearn)})
    dt <-adply(combot, 2, function(x){mycomfn(x, dtest)})
    #Delete useless variables
    dl$X1<-NULL
    dl$X<-NULL
    dt$X1<-NULL
    dt$X<-NULL
    #dichotomize dependent variable
    dl$y<-ifelse(dl$y>0,1,-1)
    dt$y<-ifelse(dt$y>0,1,-1)
    #create standard input
    xttb<-dl[,1:(ncol(dl)-1)]
    yttb<-dl$y
    xxttb<-dt[,1:(ncol(dt)-1)]
    yyttb<-dt$y
    
    #TTB-ABC:
    ##Fit TTB-ABC
    m<-ttbabcfit(xttb, yttb, 1000)
    ##Generate prediction
    checkabc[l]<-mean(dt$y==ttbabcpredict(m, xxttb)$class)
    ##Print performance of lth trial
    print(paste("ABC done. Performance: ",checkabc[l]))
    
    #TTB
    ##Fit TTB
    ###Get cases
    cases<-ncol(xttb)
    ###Order in dependency of correlation
    ttborder<-data.frame(n=1:ncol(dl[,1:cases]), dir=cor(dl[,1:cases],dl$y))
    ###if na, we assume it's 0, can happen sometimes, especially for rare cases (i.e. capital city)
    ttborder$dir<-ifelse(is.na(ttborder$dir),0,ttborder$dir)
    ###order in dependency of validity
    ttborder<-ttborder[order(abs(ttborder$dir), decreasing=TRUE),]
    ###get the weights by dichotomizing
    ttborder$dir<-ifelse(ttborder$dir>0,1,-1)
    ##Predict TTB
    ###copy dt to avoid that the changed order messes things up later
    dtcopy<-dt
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
    checkttb[l]<-mean(predttb==dt$y)
    #print performance of TTB to console
    print(paste("TTB done. Performance: ",checkttb[l]))
    }
  
  #Collect performance values, both mean and standard error over the 50 trials
  dfinal$ttbmu[k]<-mean(checkttb)
  dfinal$ttbse[k]<-standerr(checkttb)
  dfinal$abcmu[k]<-mean(checkabc)
  dfinal$abcse[k]<-standerr(checkabc)
  write.csv(dfinal, "data/citycompare2.csv")
}
#END