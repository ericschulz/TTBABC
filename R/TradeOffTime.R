#This is the last example of the TTB-ABC paper
#Copyright@Schulz, Speekenbrink, Meder
#Trading off time by adjusting nsims, proportion, and epsilon
#This script demonstrates that TTB-ABC is truly heuristic, adjusting time and performance

#Input for TTB-ABC: 
##4 trinary x variables: x_1,...,x_4 p(x_i=1)=p(x_i=-1)=1/4, p(x_i=0)=0.5
##1 binary y variable: y=TTB(x1,x2,x3) with c1>c2>c3, x4 is unimportant

#Result of script:
##traces of the 4 cue weight over time, averaged over 30 runs


#House keeping
rm(list=ls())

#Load ttbabc-sources
source("R/ttbabcfit.R")
source("R/ttbabcpredict.R")

#Load packages
packages <- c('plyr', 'gridExtra', 'microbenchmark', 'lattice')
lapply(packages, library, character.only=TRUE)

#My Take The Best-Function
myttb<-function(x){
  y<-0
  for (i in seq_along(x)){
    y<-ifelse(y==0 & x[i]!=0,x[i],y)
  }
  y<-ifelse(y==0,sample(c(-1,1),1),y)
  return(y)
}

#Create X-frame
xttb<-data.frame(
  apply(
    matrix(NA, nrow=1000, ncol=4), 2, function(x){ 
      sample(c(-1,0,0,1), 1000, replace = TRUE)}
    ))
#Create y-frame, with x_4 unimportant
yttb<-apply(xttb[,1:3],1,myttb)

#all possible combinations of to be tested proportion and epsilon-duals
m<-expand.grid(seq(0.1,0.9,0.1), seq(0.1,0.9,0.1))

#get the time for different proportion-epsilon-duals
#apply over 1,10,50,100 mental simulations
time<-lapply(c(1,10,50,100),function(x){
  #calculated over duals
  apply(m,1, function(y){
    #get the median cause it is timely data
    median(
      #microbenchmark processing time
      microbenchmark(
        #fit for given dual {y_1, y_2} and current number of simulations x
        ttbabcfit(xttb, yttb, nsims=x, epsilon = y[1], proportion=y[2], progress = FALSE), times=30)$time)
    })
})

#create data frame
dp<-data.frame(matrix(unlist(time), nrow=81), m)
#name it
names(dp)<-c(paste0("y",1:4), paste0("x",1:2))

#save for plotting
write.csv(dp, "data/timetradeoff.csv")

#Create plots, unfortunately lots of repetition here due to gridextra
##Wire frame for 1 sample
w1<-wireframe(y1~x1*x2,data=dp, main="1 Simulation",
                xlab=expression(epsilon),ylab="Prop \n sampled", zlab=expression(ns),
                drape = TRUE, col.regions = heat.colors(100),
                colorkey = TRUE, zlim=c(0,max(dp[,1:4])))

##Wire frame for 10 samples
w2<-wireframe(y2~x1*x2,data=dp, main="10 Simulations",
              xlab=expression(epsilon),ylab="Prop \n sampled", zlab=expression(ns),
              drape = TRUE, col.regions = heat.colors(255),
              colorkey = TRUE, zlim=c(0,max(dp[,1:4])))

##Wire frame for 50 samples
w3<-wireframe(y3~x1*x2,data=dp, main="50 Simulations",
              xlab=expression(epsilon),ylab="Prop \n sampled", zlab=expression(ns),
              drape = TRUE, col.regions = heat.colors(255),
              colorkey = TRUE, zlim=c(0,max(dp[,1:4])))

##Wire frame for 100 samples
w4<-wireframe(y4~x1*x2,data=dp, main="100 simulations",
              xlab=expression(epsilon),ylab="Prop \n sampled", zlab=expression(ns),
              drape = TRUE, col.regions = heat.colors(255),
              colorkey = TRUE, zlim=c(0,max(dp[,1:4])))


#Saving plot
pdf("figs/timetradeoff.pdf", width=8, height=8)
#bringing it all toegther in one plot
grid.arrange(w1,w2,w3,w4, 
             ncol=2,
             main=textGrob("Trading off time", gp=gpar(fontsize=18), vjust=0.7)
             )
dev.off()
#END