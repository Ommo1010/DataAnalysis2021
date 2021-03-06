#4a)

data=read.csv(file="Copier Maintenance.csv")
y=data$y
x=data$x
reg=lm(y~x)
res=reg$residuals
#plot(x,res,main="Residual plot",xlab="x",ylab="res")

#scatter everywhere meaning constant variance therefore 
#homogeneity of variance and normality of residuals satisfy

#4b)

#print(reg) #-0.5802+15.0352*x

#4c)

#cat(predict(reg,newdata=data.frame(x=5)))

#5a)

data=read.csv(file="Airfreight Breakage.csv")
y=data$Y
x=data$X
reg<-lm(y~x)
res=reg$residuals
#plot(x,res,main="Residual plot",xlab="x",ylab="res")

#scatter everywhere meaning constant variance therefore 
#homogeneity of variance and normality of residuals satisfy

#5b)

#print(reg) #Y=10.2+4x

#plot(x,y,main="Residual plot",xlab="x",ylab="y")
#abline(reg) #Line is a good fit 

#5c)

first=predict(reg,newdata=data.frame(x=20))
#cat(first)

#5d)

second=predict(reg,newdata=data.frame(x=10))
#cat(first-second)

#5e)

xbar=mean(x)
ybar=mean(y)
#print(ybar==10.2+4*xbar)

#7a)

data=read.csv(file="Pie Bakery.csv")
y=data$Y
x=data$X
reg=lm(y~x)
#print(summary(reg)) #p-value of estimate is greater than 0.05 significance
                    #therefore null hypothesis of B1=0 is accepted.
                    #Spoiled pies does NOT depend on temperature change.

#7b)

#cat(0.4893+2*0.8952,0.4893-2*0.8952)

#7c)

anovaresult=aov(y~x,data=data)
#print(anovaresult)

#7d)

#Yes they are because ANOVA does both variation and significance test

#8)

a=49.405-45.818
b=119-1
c=a/1
d=45.818/b
e=c/d
#cat('a'=a,'b'=b,'c'=c,'d'=d,'e'=e)
#cat(pf(e,1,b,lower.tail=F)) #Reject H0 because p-vale<0.05




