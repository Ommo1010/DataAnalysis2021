samples<-replicate(100000,rnorm(1,4,0.5))
h<-hist(samples) 
b<-cut(1,h$breaks)
clr<-ifelse(h$breaks<4.074,"green","red")[-length(h$breaks)]#highlight
clr[b]<-"red"
hist(samples,probability=TRUE,col=clr)#sample distribution
curve(dnorm(x,4,0.5),add=TRUE) #theoretical distribution