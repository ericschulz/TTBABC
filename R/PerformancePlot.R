rm(list=ls())

setwd("/home/hanshalbe/Desktop/TTBABC")
rm(list=ls())
packages <- c('ggplot2', 'gridExtra')
lapply(packages, require, character.only = TRUE)


d1<-read.csv("data/citycross.csv")
d2<-read.csv("data/homelesscross.csv")
d3<-read.csv("data/salarycross.csv")


dplot1<-data.frame(mu=as.vector(as.matrix(d1[,2:7])),
                   se=as.vector(as.matrix(d1[,8:13])),
                   proportion=rep(seq(0.1,0.9,0.1), 6),
                   Model=rep(c("TTB-ABC", "Classic", "Updated"), each=18),
                   Method=rep(rep(c("Performance", "Distance"), each=9), 3))



dplot2<-data.frame(mu=as.vector(as.matrix(d2[,2:7])),
                   se=as.vector(as.matrix(d2[,8:13])),
                   proportion=rep(seq(0.1,0.9,0.1), 6),
                   Model=rep(c("TTB-ABC", "Classic", "Updated"), each=18),
                   Method=rep(rep(c("Performance", "Distance"), each=9), 3))

dplot3<-data.frame(mu=as.vector(as.matrix(d3[,2:7])),
                   se=as.vector(as.matrix(d3[,8:13])),
                   proportion=rep(seq(0.1,0.9,0.1), 6),
                   Model=rep(c("TTB-ABC", "Classic", "Updated"), each=18),
                   Method=rep(rep(c("Performance", "Distance"), each=9), 3))




pd <- position_dodge(.1)

p1<-ggplot(dplot1, aes(x=proportion, y=mu, colour=Model)) +
  geom_errorbar(aes(ymin=mu-se, ymax=mu+se), width=0.2, size=0.5, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd)+xlab("")+ggtitle("City Size")+
  ylab("Performance")+theme(text = element_text(size=16))+
  facet_wrap(~ Method, scales="free", nrow=1)
  
p2<-ggplot(dplot2, aes(x=proportion, y=1-mu, colour=Model)) +
  geom_errorbar(aes(ymin=1-mu-se, ymax=1-mu+se), width=0.2, size=0.5, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd)+xlab("")+ggtitle("Homelessness")+
  ylab("Performance")+theme(text = element_text(size=16))+
  facet_wrap(~ Method, scales="free", nrow=1)

  

p3<-ggplot(dplot3, aes(x=proportion, y=mu, colour=Model)) +
  geom_errorbar(aes(ymin=mu-se, ymax=mu+se), width=0.2, size=0.5, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd)+xlab("")+ggtitle("Professors' salary")+
  ylab("Performance")+theme(text = element_text(size=16))+
  facet_wrap(~ Method, scales="free", nrow=1)

pdf("figs/perfmonance.pdf")
grid.arrange(p1,p2,p3, nrow=3,main=textGrob("Performance",gp=gpar(fontsize=18),
                                        vjust=0.7))
dev.off()  