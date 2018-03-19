par(mfrow=c(2,4))
data = read.table("africa.txt",header=TRUE)
attach(data)
#1

sample1 = rpois(20,5)
plot(sample1);#lines(lowess(sample1))
qqnorm(sample1);qqline(sample1)
sample2 = rpois(30,7.5)
plot(sample2);#lines(lowess(sample2))
qqnorm(sample2);qqline(sample2)
sample3 = rpois(40,10)
plot(sample3);#lines(lowess(sample3))
qqnorm(sample3);qqline(sample3)
sample4 = rpois(50,12.5)
plot(sample4);#lines(lowess(sample4))
qqnorm(sample4);qqline(sample4)
#not sure what to say here, but as the lambda value increases (2nd paramater) the distribution looks more normal (as expected)


#2
#this part is more theoretical, I need to look into it.

#3
africaglm=glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size+numelec+numregim,family=poisson,data=data) 
summary(africaglm) 

#4
#Using step down approach each variable from the right is removed
summary(glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size+numregim,family=poisson,data=data))
summary(glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size,family=poisson,data=data))
summary(glm(miltcoup~oligarchy+pollib+parties+pctvote+popn,family=poisson,data=data))
summary(glm(miltcoup~oligarchy+pollib+parties+pctvote,family=poisson,data=data))
summary(glm(miltcoup~oligarchy+pollib+parties,family=poisson,data=data))
#Following this approach, the new model is: miltcoup = 0.251377 + 0.092622*oligarchy - 0.574103*pollib + 0.22059*parites + error
#5
model = glm(miltcoup~oligarchy+pollib+parties,family=poisson,data=data)
#diagonistic plots
plot(fitted(model),residuals(model));lines(lowess(fitted(model),residuals(model)))
plot(log(fitted(model)),residuals(model));lines(lowess(log(fitted(model)),residuals(model)))
plot(fitted(africaglm), residuals(africaglm));lines(lowess(fitted(africaglm),residuals(africaglm)))
plot(log(fitted(africaglm)),residuals(africaglm));lines(lowess(log(fitted(africaglm)),residuals(africaglm)))
#I cannot see any pattern