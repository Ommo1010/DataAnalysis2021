temperature<-c(86,87,83,92,67,77,75,74,92,94,82,77,80,66,78,82,93,96,86,79)
p<-c(3,6,9,12)
z<-c(0.1,0.35,0.5,0.99)
x<-seq(-4,10,by=0.25)

Normal.P<-function(p) {
  y<-1-pnorm(p,mean=3,sd=3)
  print(y) #Question 6.a.
}

Normal.Z<-function(z) {
  y<-qnorm(z,3,3)#Question 6.b.
  print(y)
}

Graphs.hist<-function(temperature) {
  hist(temperature) #Question 5.b.
}

Graphs.box<-function(temperature) {
  boxplot(temperature) #Question 5.c.
}

Normal.pdf<-function(x) {
  plot(dnorm(x, mean = 3, sd = 3),main="plot of pdf") #Question 6.c.
}

Normal.cdf<-function(x) {
  plot(pnorm(x, mean = 3, sd = 3),main="plot of cdf") #Question 6.c.
}

Normal.PP.PLot<-function(temperature) {
  Sorted.Data<-sort(temperature)
  J <-seq(1,length(temperature),1)
  Z.Value<-qnorm((J-0.5)/length(temperature))
  Normal.Plot<-plot(Z.Value,Sorted.Data)
  Normal.Plot #Question 5.c.
}

Random<-function() {
  print(rt(10,2)) #Question 6.d.
}

#Normal.PP.PLot(temperature)
#Normal.P(p)
#Graphs.hist(temperature)
#Graphs.box(temperature)
#Normal.pdf(x)
#Normal.cdf(x)
Random()
#Normal.Z(z)