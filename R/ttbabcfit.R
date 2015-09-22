#This is a function to fit TTB-ABC
#Copyright@Schulz, Speekenbrink & Meder
#ttbabc:: (data.frame, vector, numeric, numeric, numeric, (numeric,numeric), logigal)->list(data.frame, data.frame, data.frame)

#INPUT:
#X: A matrix or data frame that consists of the comparisons between binary cues, X={-1=loss,0=draw,1=win}
#y: A vector of the results (dependent variable), can either be 1 (first item won) or -1 second item won
#epsilon: a scalar indicating the acceptance threshold, 0<=epsilon<=1, default=0.9, so relatively high
#proportion: number of sampled cases to check simulations, 0<=proportion<=1, default=0.1, so 10%
#countprior: initializes the pseudo-count for the beta binomial-update, default is 25 successes in 50 trials
#progress: logical value indicating the presence or absence of a progress bar, default=TRUE

#OUTPUT:
#list:
#$m
##$balls: cue number
##$ballcounts: number of balls in polya urn
##$wsucc: number of positive weights
##$wtrials: number of trials
##$abc: data.frame with traces of cue-choice probabilities
##$dirs: data.frame with traces of cue-directions
##trials: scalar with the overall number of trials, will equal nsims


ttbabcfit<-function(X, y, nsims=100, epsilon=0.9, proportion=0.1, nsamples=100, countprior=c(25,50), progress=TRUE){
  
  #First, perform some checks:
  ##X-values have to be either -1,0, or 1 for loss, draw, win
  if (sum(X!=-1 & X!=0 & X!=1)>0){stop("Wrong X-input. X-values have to be {-1,0,1}.")}
  ##y has to be either -1 or 1 for lost or won comaprison
  if (sum(y!=-1 & y!=1)>0){       stop("Wrong y-input. y-vector has to be {-1,1}.")}
  ##nsims has to be an integer bigger than 0
  if (nsims<=0 || nsims !=round(nsims)) {stop("Wrong nsims input. nsims has to be an integer bigger than 0.")}
  ##epsilon has to be a probability
  if (epsilon<0 || epsilon > 1) {stop("Wrong epsilon input. Acceptance rate can never be smaller than 0 or bigger than 1.")}
  ##epsilon has to be a probability
  if (proportion<0 || proportion > 1) {stop("Wrong proportion input. Proportion of sampled data can never be smaller than 0 or bigger than 1.")}
  ##epsilon has to be a probability
  if (countprior[1]>countprior[2]) {stop("Wrong countprior input. The first pseudo count has to be bigger than the second.")}
  ##progress has to be logical
  if (!is.logical(progress)) {stop("Wrong progress input. The variable indicating the presence of a progress bar has to be logical.")}
  
  
  #number of cues
  cases<-ncol(X)
  #initialize data frame with pseudo-counts for both cue importance and direction
  polyacue<-data.frame(balls=1:cases, ballcounts=countprior[1], wsucc=countprior[1], wtrials=countprior[2])
  #initialize overall count
  a<-0
  #progress bar, if asked for
  if(progress){
    pb <- txtProgressBar(min=a, max=nsims, style=3)
  }
  #pseudo-trials
  trials<-countprior[2]
  #create combined data.frame
  d<-cbind(X,y)
  #get rid of combinatory garbage
  d$X<-NULL
  #create a collector frame for traces of cue importance
  collect<-data.frame(matrix(1/cases, nrow=nsims, ncol=cases))
  #create a collector frame for traces of cue direction
  weights<-data.frame(matrix(0, nrow=nsims, ncol=cases))
  
  #start simulation
  while (a<nsims){ 
    #creat probability of tree position by Beta-Binomial-posterior process
    pchoice<-apply(cbind(polyacue$ballcounts, trials), 1, function(x){rbeta(1, 0.5+x[1], 0.5+x[2]-x[1])})
    #select a randomly drawn cue order in dependency of the tree position probability
    select<-sample(1:cases,cases,prob=pchoice/sum(pchoice), repl=FALSE)
    #create a dummy urn
    polyasub<-polyacue[select,]
    #create a proposal tree
    ttbprop<-data.frame(cue=polyasub$balls)
    #create weight probabilities by posterior beta-binomial process
    pweights<-apply(cbind(polyasub$wsucc, polyasub$wtrials), 1, function(x){rbeta(1, 0.5+x[1], 0.5+x[2]-x[1])})
    #create proposal weights in dependency of the weight balls
    ttbprop$w<-ifelse(rbinom(nrow(ttbprop),1, prob=pweights/sum(pweights))==0,-1,1)
    #sample one comparison from learning set of the size given by input proportion
    #dprop<-d[sample(1:nrow(d), round(proportion*nrow(d))),]
    dprop<-d[sample(1:nrow(d), nsamples, replace = TRUE),]
    #initialize the y-proposal
    yprop<-rep(0, nrow(dprop))
    #generate predictions for y proposal
    for (i in 1:(nrow(ttbprop))){
      yprop<-ifelse(yprop==0,ttbprop$w[i]*dprop[,ttbprop$cue[i]],yprop)
    }
    #if all are X's are 0s, then guess
    yprop<-ifelse(yprop==0,ifelse(rbinom(1,1,0.5)==0,-1,1),yprop)
    #create the proposal statistic, that is how well the tree performs in the sampled set
    proposal<-mean(yprop==dprop$y)
    #chech if proposal statistic exceeds or espilon, i.e. is the simulation good enough?
    if (epsilon < proposal){
      #increment trials
      trials<-trials+1
      #increment tracker
      a<-a+1
      #update progress bar, if asked for
      if(progress){
        setTxtProgressBar(pb, a)
      }
      #update the collection of the cue importance traces
      collect[a,1:cases]<-polyacue$ballcounts/sum(polyacue$ballcounts)
      #update the collection of the cue direction traces
      weights[a,1:cases]<-(polyacue$wsucc/polyacue$wtrials-0.5)*2
      #mark the cue that actually made the difference
      mark<-which(ttbprop$cue %in% which(dprop[,1:cases]!=0))[1]
      mark<-ifelse(is.na(mark),sample(1:cases,1),mark)
      #reinforce successful tree up to that cue via polya scheme
      polyasub$ballcounts[1:mark]<-polyasub$ballcounts[1:mark]+1
      #reinforce successful weights up to that cue via polya scheme
      polyasub$wsucc[1:mark]<-polyasub$wsucc[1:mark]+ifelse(ttbprop$w[1:mark]==1,1,0)
      #update weight trials in dependency of their direction
      polyasub$wtrials[1:mark]<-polyasub$wtrials[1:mark]+1
      #proposed sub becomes new polya urn
      polyacue<-polyasub
      #order please!
      polyacue<-polyacue[order(polyacue$balls),]
      #house keeping for memory sake
      rm(dprop,polyasub)
      
    }
  }
  #close progress bar, if existent
  if(progress){close(pb)}
  
#return the final list with the posterior model m, the cue importance traces abc, and the cue direction traces dirs 
return(list(m=polyacue, abc=collect, dirs=weights, trials=trials))
}