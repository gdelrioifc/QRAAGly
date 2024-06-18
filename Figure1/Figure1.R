library(scales)
setwd("/Volumes/Disco2024/Papers/EnPreparacion/DProtein/2024/EAAInProteinsAndMetabolism/DataFiguresPaper/Figure1")

par(mfrow=c(2,3))

data1 <- read.csv("dataFigure1.csv")

plot.aminodist <- function(x,labels,color,df,lim,size,alpha) 
{
  n <- length(colnames(df))
  y <- df[,1]
  plot(x,y,pch=16,cex=size,xaxt='n',xlab="", ylab="",col=alpha(color,alpha), ylim=lim)
  axis(1,at=x,labels=labels,las=2)
  apply(df[,2:n],MARGIN=2,FUN=function(y) {points(x,y,pch=16,cex=size,col=alpha(color,alpha))})
}

lim1=c(0,10)
plot.aminodist(data1[,1],data1[,2],c(rep('blue',9),rep('red',11)),data1[,c(3,4,15,16,17,18)],lim1,0.75,0.2)
l1=c(paste0('Insects'))
mtext(l1, side=3, line=0, cex=1.5, font=3)
l2=c(paste0('RQAA'))
mtext(l2, side=2, line=2.2, cex=1.5, font=3)

lim2=c(0,20)
plot.aminodist(data1[,1],data1[,2],c(rep('blue',9),rep('red',11)),data1[,c(5,21,22,23,27,28,34,35,53,54,55)],lim2,0.75,0.2)
l1=c(paste0('Plants'))
mtext(l1, side=3, line=0, cex=1.5, font=3)

lim3=c(0,1.5)
plot.aminodist(data1[,1],data1[,2],c(rep('blue',9),rep('red',11)),data1[,c(6,33,52)],lim3,0.75,0.2)
l1=c(paste0('Fish'))
mtext(l1, side=3, line=0, cex=1.5, font=3)

lim4=c(0,8)
plot.aminodist(data1[,1],data1[,2],c(rep('blue',9),rep('red',11)),data1[,c(7,8,9,10,11,12,13,14,19,20,24)],lim4,0.75,0.2)
l1=c(paste0('Bacteria'))
mtext(l1, side=3, line=0, cex=1.5, font=3)
l2=c(paste0('RQAA'))
mtext(l2, side=2, line=2.2, cex=1.5, font=3)

lim5=c(0,30)
plot.aminodist(data1[,1],data1[,2],c(rep('blue',9),rep('red',11)),data1[,c(25,29,31,32,38,39,40,41,42,43,44,45,46,47,48,49,50,51)],lim5,0.75,0.2)
l1=c(paste0('Fungi'))
mtext(l1, side=3, line=0, cex=1.5, font=3)

lim6=c(0,10)
plot.aminodist(data1[,1],data1[,2],c(rep('blue',9),rep('red',11)),data1[,c(26,30,36,37,56,57,58,59,60)],lim6,0.75,0.2)
l1=c(paste0('Animals'))
mtext(l1, side=3, line=0, cex=1.5, font=3)

