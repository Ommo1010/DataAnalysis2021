n<<-16 #sample size
reps<<-100000 
pop.mean<<-20
pop.variance<<-64

sample.distribution<-function() {
  samples<-replicate(reps,rnorm(n,pop.mean,sqrt(pop.variance)))
  sample.avgs<-colMeans(samples) #sample mean
  
  hist(sample.avgs,ylim=c(0,0.2),freq=FALSE) #sample distribution
  curve(dnorm(x,20,sqrt(pop.variance)/sqrt(n)),add=TRUE) #theoretical distribution
}

sample.boundaries<-function() {
  for (val in seq(0,10,0.1)) {
    z.score1<-((20-val)-pop.mean)/(sqrt(pop.variance)/sqrt(n))
    z.score2<-((20+val)-pop.mean)/(sqrt(pop.variance)/sqrt(n))
    if((pnorm(z.score1)+(1-pnorm(z.score2)))<0.0027)
      break
  }
  cat("b1 is:",20-val,", b2 is:",20+val,sep='')
}

sample.boundaries1<-function() {
  lower<-qnorm(0.0027/2,20,2,lower.tail=TRUE) #sample sd=2=sqrt(pop.variance)/sqrt(n) 
  upper<-qnorm(0.0027/2,20,2,lower.tail=FALSE)
  cat("b1 is:",lower,", b2 is:",upper,sep='')
}

sample.distribution() #5.a.
#sample.boundaries() #5.b. bad method
sample.boundaries1() #5.b. good method
