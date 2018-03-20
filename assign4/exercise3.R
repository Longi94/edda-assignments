par(mfrow=c(2,3))
data = read.table("africa.txt",header=TRUE)
attach(data)
#1

n = c(50,150,300)
lambda = c(1,10,50)
for(i in n){
  for(j in lambda){
    data_generated = rpois(i,j)
    boxplot(data_generated, main=c("n",i,"lambda",j))
    qqnorm(data_generated, main=c("n",i,"lambda",j));qqline(data_generated)
  }
}



#2
#this part is more theoretical.

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
par(mfrow=c(1,3))
model = glm(miltcoup~oligarchy+pollib+parties,family=poisson,data=data)
#diagonistic plots
plot(fitted(model),residuals(model))
plot(log(fitted(model)),residuals(model))
plot(log(fitted(model)),residuals(model, type="response"))
plot(fitted(africaglm), residuals(africaglm))
plot(log(fitted(africaglm)),residuals(africaglm))
plot(log(fitted(africaglm)),residuals(africaglm, type="response"))
