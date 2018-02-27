par(mfrow=c(2,2))
data = read.table("run.txt", header=TRUE)
before = data[,1]
after = data[,2]
soft_d_before = before[1:12]
soft_d_after = after[1:12]
energy_d_before = before[13:24]
energy_d_after = after[13:24]
drink_type = data[,3]

#Q1
min(before);mean(before);max(before);median(before)
min(after);mean(after);max(after);median(after)

plot(data)

hist(before, freq=FALSE); lines(density(before), col="red",lwd=2)
hist(after, freq=FALSE); lines(density(after), col="red", lwd=2) 


#Q2
#test difference in speed before and after soft drink
t.test(soft_d_before, soft_d_after, paired=TRUE)

#test difference in speed before and after energy drink
t.test(energy_d_before, energy_d_after, paired=TRUE)

#Q3 
 #computing differences
 diffarr = numeric(length(before))
 for(i in 1:length(before)){
   diffarr[i] = abs(after[i]-before[i])
 }
 
 drinkframe=data.frame(difference=diffarr, variety=factor(data[,3]))
 
 drinkaov = lm(difference~variety,data=drinkframe)
 anova(drinkaov)
 summary(drinkaov)
 confint(drinkaov)
 #p has a low value, 
 
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
 qqnorm(residuals(drinkaov)); qqline(residuals(drinkaov))
 