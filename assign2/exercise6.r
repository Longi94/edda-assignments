par(mfrow=c(2,2))
data = read.table("../Data/run.txt", header=TRUE)
before = data[,1]
after = data[,2]
soft_d_before = before[1:12]
soft_d_after = after[1:12]
energy_d_before = before[13:24]
energy_d_after = after[13:24]

plot(before~after,pch=grp,col=grp,data=data); abline(0,1)
boxplot(before,after)
boxplot(before-after)

#test difference in speed before and after soft drink
t.test(soft_d_before, soft_d_after, paired=TRUE)

#test difference in speed before and after energy drink
t.test(energy_d_before, energy_d_after, paired=TRUE)
