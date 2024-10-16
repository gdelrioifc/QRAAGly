library(scales)
setwd("<include_your_working_dir_here>")

plot.aminodist <- function(x,labels,color,df,size,alpha) 
{
  n <- length(colnames(df))
  y <- df[,1]
  plot(x,y,pch=16,cex=size,xaxt='n',xlab="", ylab="",col=alpha(color,alpha), ylim=c(0.0,80))
  axis(1,at=x,labels=labels,las=2)
  apply(df[,2:n],MARGIN=2,FUN=function(y) {points(x,y,pch=16,cex=size,col=alpha(color,alpha))})
}

data1 <- read.csv("dataFigure1.csv")

plot.aminodist(data1[,1],data1[,2],c(rep('blue',9),rep('red',11)),data1,0.75,0.2)
l2=c(paste0('RQAA'))
mtext(l2, side=2, line=2.2, cex=1.5, font=3)
