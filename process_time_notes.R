n0=c(1000,10000,20000,30000,40000,50000,60000,70000)
n1=n0/10000
process_time=c(0.31,6,18.5,35,82.7,156,309,425)

y=process_time
x=n0
f1x=exp(n1)

summary(lm(y~x))
plot(x,y)
boxplot(y)

y=process_time
x=n1
f1x=exp(n1)

summary(lm(y~f1x))

plot(x,y)+lines(x,f1x,col="red")

f2x=0.75185*exp(n1)+18.28966

plot(x,y)+lines(x,f2x,col="red")+lines(x,f1x,col="green")

lower=1
upper=8
partial_x<-n1[lower:upper]
partial_y<-process_time[lower:upper]
partial_f1x=exp(partial_x)
#summary(lm(y~f1x))
output=summary(lm(partial_y~partial_f1x))
output
ceoff0=output$coefficients[1]
ceoff1=output$coefficients[2]
x=n1
y=process_time
f1x=ceoff0+ceoff1*exp(x)
plot(x,y)+lines(x,f1x,col="red")
#(ceoff0+ceoff1*exp(15.6))/(3600*24)
#ceoff0+ceoff1*exp(6)

d=5
lower=5
upper=8
partial_x<-n1[lower:upper]
partial_y<-process_time[lower:upper]
partial_f1x=partial_x^d
#summary(lm(y~f1x))
output=summary(lm(partial_y~partial_f1x))
output
ceoff0=output$coefficients[1]
ceoff1=output$coefficients[2]
x=n1
y=process_time
f1x=ceoff0+ceoff1*x^d
plot(x,y)+lines(x,f1x,col="red")
#(ceoff0+ceoff1*15.6^d)/(3600*24)
ceoff0+ceoff1*7^d


















