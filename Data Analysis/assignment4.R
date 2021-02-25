print(pnorm(1.9599644)-pnorm(-1.9599644))#1a
print(qnorm(.90))#1b
print(pnorm(2.575829))*100#1c
print(20-qnorm(.05)*(2/400))#1dleft
print(20+qnorm(.05)*(2/400))#idright

data<-c(55.291,59.718,62.688,63.886,
        66.458,68.548,69.857,70.833,
        70.237,69.787,68.793,66.338,
        65.454,64.391,63.620,63.287)

qqnorm(data,main="PP plot of Solar Energy Consumed")
qqline(data)#3a
print(t.test(data, conf.level = 0.9))#3b

syrup=c(3.29, 1.94, 3.14, 2.82, 2.15, 
        1.94, 3.85, 3.36, 3.10, 1.47, 
        3.44, 2.62, 3.63, 1.37, 1.72, 
        3.15, 1.78, 1.16, 3.63, 2.13, 
        1.30, 3.02, 1.46, 1.49, 2.09)
print(t.test(syrup))#4
