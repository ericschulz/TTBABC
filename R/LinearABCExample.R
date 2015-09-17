#This is example 1 from the TTB-ABC paper
#Copyright@Schulz, Speekenbrink, Meder
#Recovering a simple linear regression with Approximate Bayesian Computation

#House keeping
rm(list=ls())

#Load packages
packages <- c('plyr', 'ggplot2', 'gridExtra')
lapply(packages, library, character.only=TRUE)

#standard error
myse<-function(x){
  y<-sd(x)/sqrt(length(x))
  return(y)
}

#number of observations
n <- 1000
#true beta
beta1<-1
#noise
x1 <- rnorm(n,0,1)
#observations
y1 <- beta1*x1+rnorm(1000,0,0.1)

#prior
prior<-runif(100000,-5,5)

#Initialize distance
d<-m<-rep(0,length(prior))
for (i in 1:length(prior)){
  #plug in proposals from prior
  yprop<-prior[i]*x1+rnorm(1000,0,0.1)
  #calculate distance
  d[i]<-mean((yprop-y1)^2)
}


#now let's track epsilon, criterion, mean posterior, and standard error
epsilon<-crit<-mu<-se<-seq(50,2,len=100)
#over all criteria
for (i in 1:length(mu)){
  #change epsilon
  epsilon[i]<-(crit[i])
  #calculate mu for that epsilon
  mu[i]<-mean(prior[d<(crit[i])], na.rm=TRUE) 
  #Calculate standard error for that epsilon
  se[i]<-myse(prior[d<(crit[i])]) 
}

#Collect all into data frame
mydat<-data.frame(mu=rev(mu),se=rev(se),crit=crit)

#do the plots
pd <- position_dodge(.1)

#produced poterior plot
posterior<-ggplot(mydat, aes(x=crit, y=mu)) +
  #errorbars
  geom_errorbar(aes(ymin=mu-se, ymax=mu+se), width=0.05, position=pd) +
  #lines
  geom_line(position=pd, width=0.5) +
  #xlab, a little complicated, but easier to track
  xlab(expression(50-epsilon))+
  #ylab
  ylab(expression(mu))+
  #title
  ggtitle("Approximated posterior")

#distances overall
dist<-qplot(d, geom="histogram")+ geom_histogram(aes(fill = ..count..))+
  ylab("Count")  +xlab("Distance")+ggtitle("Produced distances")

#create data frame for line plot
dat<-data.frame(x=x1,y=y1)
#plot function
func<-ggplot(dat, aes(x=x, y=y)) +
  #all points
  geom_point(shape=1) + 
  #linear regression line
  geom_smooth(method=lm, se=FALSE) +
  #title
  ggtitle("Underlying data")

#Now, let's save all of it as a grid
pdf("figs/linexample.pdf", width=8, height=12)
grid.arrange(func, dist, posterior)
dev.off()

