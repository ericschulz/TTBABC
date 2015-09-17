#This is Example 3 from the TTB-ABC paper
#Copyright@Schulz, Speekenbrink & Meder

#We will check if TTB-ABC is able to make sound inference in the city size data set
#This script demonstrates that TTB-ABC can succesfully infer cue orders for a classic data set
#The city size data set is a frequently used data set in the literature, see Gigerenzer & Goldstein (1999)
#Input for TTB-ABC: 
##8 trinary x variables: Soccer_Team, State_Capital, Former_East_Germany, 
##                       Industrial_Belt, Licence_plate, Intercity_trainline, 
##                       Exposition_Site, National_Capital, University
##1 binary y variable: city size(population size)

#House keeping
rm(list=ls())

#Load TTB-ABC
source("R/ttbabcfit.R")

#Load packages
packages <- c('ggplot2', 'plyr')
lapply(packages, library, character.only = TRUE)

#read in city data
city<-read.csv("data/city.csv")

#generate all possible pairwise comparisons
combos <- combn(nrow(city), 2)

#function to get differences of pairwise comparisons
mycomfn<-function(x){
  outframe<-data.frame(city[x[1],] - city[x[2],])
return(outframe)
}

#generate data set with all pairwise comparisons from city data
city<-adply(combos, 2, mycomfn)

#Delete useless variables
city$X1<-NULL
city$X<-NULL

#dichotomize dependent variable
city$y<-ifelse(city$y>0,1,-1)

#Get the usual format
xttb<-city[,-ncol(city)]
yttb<-city[,ncol(city)]

#set seed to eric's birthday
set.seed(310887)

#30 trials of 100 simulations
trial<-rep(100, 50)

#get traces for 30 trials with 100 simulations, TAKE CARE: this will run for a while...
dout<-lapply(trial, function(x){ttbabcfit(xttb, yttb, x, epsilon=0.65, proportion=0.01)})

#get the mean for each sim point over all 30 trials
##apply to all cases
mul<-lapply(1:ncol(xttb), function(x){
  #the mean
  apply( 
    #unlisted
    sapply( 
      #get element
      lapply(dout,function(y) {y$abc[,x]}),unlist), 1, mean
  )
})

#get the mean for each sim point over all 30 trials
##apply to all cases
sel<-lapply(1:ncol(xttb), function(x){
  #the standard error
  apply( 
    #unlisted
    sapply( 
      #
      lapply(dout, function(y) {y$abc[,x]}), unlist), 1, function(z){
        #standard error
        sd(z)/sqrt(length(z))}
  )
})  

#Get rid of uncerscores in cue names
cuenames<-gsub("_", " ", names(xttb))
#Plot
dplot<-data.frame(mu=unlist(mul), se=unlist(sel),  trials=rep(1:100, ncol(xttb)), Cue=rep(cuenames, each=100))

#Position of points
pd <- position_dodge(.1)

#Create plot:
##mu(p) over trials per Cue
p<-ggplot(dplot, aes(x=trials, y=mu, color=Cue)) + 
  #black line
  geom_line(position=pd, size=0.5)+
  #points with bigger size
  geom_point(position=pd, size=1)+
  #error-bars
  geom_errorbar(aes(ymin=mu-se, ymax=mu+se), width=0.5, size=0.5, position=pd, alpha=0.2) +
  #xlab
  xlab("Trials")+
  #ylab
  ylab("Probabilities")+
  #title
  ggtitle("Trace Plot")+
  #white theme
  theme_bw()+
  #bigger fonts
  theme(text = element_text(size=22))

#What does it look like?
print(p)

#save pdf
pdf("figs/citytrace.pdf")
print(p)
dev.off()
#END