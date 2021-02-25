data<-c(3.29,1.94,3.14,2.82,2.15,1.94,3.85,3.36,3.10,1.47,
        3.44,2.62,3.63,1.37,1.72,3.15,1.78,1.16,3.63,2.13,
        1.30,3.02,1.46,1.49,2.09)
sample.mean<<-mean(data)
sample.length<<-length(data)
m0<<-2.5 #mean under null hypothesis
alpha<<-0.05
question2<-function(V){
  sigma<-sqrt(V)
  S<-sqrt(V/sample.length)
  statistic<-abs((sample.mean-m0)/S)
  p<-2*pnorm(statistic,lower.tail = FALSE)
  LCL<-(sample.mean-S*qnorm(1-alpha/2))
  UCL<-(sample.mean+S*qnorm(1-alpha/2))
  print(list(statistic=statistic,p.value=p,LCL=LCL,UCL=UCL))
}
question2(V=1.5^2)

energy<-c(55.291, 59.718, 62.688, 63.886, 66.458, 68.548, 
          69.857, 70.833, 70.237, 69.787, 68.793, 66.338, 
          65.454, 64.391, 63.620, 63.287)
x=t.test(energy,mu=62,alternative = "greater")
y=power.t.test(n=16,del=65.57412-62,sd=sd(energy),sig.level=.05,
               type="one.sample",alternative="one.sided")

mood1<-c(3,0,6,7,4,3,2,1,4)
mood2<-c(5,1,5,7,10,9,7,11,8)
x=t.test(mood1,mood2,paired=TRUE,alternative="two.sided")
y=power.t.test(n=9,delta=-3.6667,sd=3.5,sig.level=.05,
               type="one.sample",alternative="two.sided")
z=power.t.test(d=3.6667/3.5,power=.85, sig.level = .05,type="paired")

sigma0<-17.5
sigma1<-20.1
mean0<-128.2
mean1<-126.5
n0<-1623
n1<-1911
m0=0

ratio<-sigma0^2/sigma1^2
sp=sqrt(((n0-1)*sigma0^2+(n1-1)*sigma1^2)/(n0+n1-2))
z=(mean0-mean1)/(sp*sqrt(1/n0+1/n1))#zvalue
ztest=qnorm(p=.05/2,lower.tail=T)#c
x=2*pnorm(-abs(z))#pvalue

S=sqrt((sigma0^2/n0)+(sigma1^2/n1))
statistic=(mean0-mean1-m0)/S
xx=2*pnorm(abs(statistic),lower.tail=FALSE)
LCL=(mean0-mean1-S*qnorm(1-0.05/2))
UCL=(mean0-mean1+S*qnorm(1-0.05/2))
