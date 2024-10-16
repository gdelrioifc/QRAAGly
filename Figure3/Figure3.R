library(scales)

setwd("/Volumes/Disco2024/Papers/EnPreparacion/DProtein/2024/EAAInProteinsAndMetabolism/DataFiguresPaper/Figure3")

df<-read.csv("dataFigure3.csv")

plot.aminodist <- function(x,labels,color,df,size,alpha) 
{
  n <- length(colnames(df))
  y <- df[,1]
  plot(x,y,pch=16,cex=size,xaxt='n',xlab="", ylab="RQAA",col=alpha(color,alpha), ylim=c(0,2.5))
  axis(1,at=x,labels=labels,las=2)
  apply(df[,2:n],MARGIN=2,FUN=function(y) {points(x,y,pch=16,cex=size,col=alpha(color,alpha))})
}
plot.aminodist(df[,1],df[,2],c(rep('blue',9),rep('red',11)),df[,-(1:2)],0.75,0.2)
