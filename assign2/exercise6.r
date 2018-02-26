par(mfrow=c(2,2))
data = read.table("run.txt", header=TRUE)
before = data[,1]
after = data[,2]
soft_d_before = before[1:12]
soft_d_after = after[1:12]
energy_d_before = before[13:24]
energy_d_after = after[13:24]

#Q1
hist(after, freq=FALSE); lines(density(after), col="red", lwd=2) 
hist(before, freq=FALSE); lines(density(before), col="red",lwd=2)
qqnorm(after); qqline(after)
qqnorm(before); qqline(before)

hist(soft_d_after, freq=FALSE); lines(density(soft_d_after), col="red", lwd=2) 
hist(soft_d_before, freq=FALSE); lines(density(soft_d_before), col="red",lwd=2)
qqnorm(soft_d_after); qqline(soft_d_after)
qqnorm(soft_d_before); qqline(soft_d_before)

#Q2
#test difference in speed before and after soft drink
t.test(soft_d_before, soft_d_after, paired=TRUE)

#test difference in speed before and after energy drink
t.test(energy_d_before, energy_d_after, paired=TRUE)

#Q3 N.B. I think the right test has to be the permutation test (below) but in Q6 we must state which distribution we assume here. See lecture 4 two paired samples
tstat = function(x,y){mean(x-y)}
B = 1000
tstar = numeric(B)
for(i in 1:B){
  tempstar = t(apply(cbind(before,after), 1, sample))
  tstar[i] = tstat(tempstar[,1], tempstar[,2])
}

 myt= tstat(before, after)
 hist(tstar)
 pl=sum(tstar<myt)/B
 pr=sum(tstar>myt)/B
 p=2*min(pl,pr)
 p 
 #p has a high value, approx 0.9 meaning that we can reject H0 in this case. There time differences are not affected by the type of drink.
 
 #N.B. not sure if I understood correctly but there is no need for code here. Just reasoning.
 
 #Q4
 #There would be no need for the softdrinks, which would give us a bigger sample size for energy drinks.
 
 #Q5
 #Yes the objection is similar as the participants should not have been split, but rather all of them should have drank the energy drink and an and
 # then drank the soft drink and ran, so that you can compare time differences for the same person since different people might simply have different running speed.
 #Furthermore, maybe the initial run (without drinking energy drinks or softdrinks) could have been removed since this would mean that the participants might be too tired for the 3rd run.
 
 
 #Q6
 #N.B. I'm not sure what the is meant by assumed distribution. I went through the notes and I'm quite sure that a permutation test is required
 #In order to change the vector values to residuals we can apply a linear regression model on the vector
 residuals = lm(tstar[,2]~tstar[,1])
 #considering that QQplots are specified, then the assumed distribution should be Normal.
 qqnorm(resid(residuals))
 qqline(resid(residuals))
 