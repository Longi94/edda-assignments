par(mfrow=c(2,2))
data = read.table("telephone.txt", header=TRUE)
data = data[,1]

x=seq(0,max(data),length=1000)

t_stat = median(data)
B = 1000
tstar = numeric(B)
n = length(data)
for(i in 1:B){
 xstar = rexp(n,c(0.01,0.1))
 tstar[i] = median(xstar)
}

hist(tstar, prob=T)
pl = sum(tstar<t_stat)/B
pr=sum(tstar>t_stat)/B
p = 2*min(pl,pr)
p

#p is always below 0.05 and therefore it does not stem from the exponential distribution. 
#Furthermore, the histogram resembles more of a bell-shaped curve rather than an exponential one

