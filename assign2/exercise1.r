par(mfrow=c(2,2))
data = read.table("telephone.txt", header=TRUE)
data = data[,1]

x=seq(0,max(data),length=1000)

t_stat = median(data)
B = 1000
tstar = numeric(B)
n = length(data)

seqconst = seq(0.01,0.1,0.001)
constindex = 1
parr = numeric(length(seqconst))

for(j in seqconst){
  for(i in 1:B){
    xstar = rexp(n,j)
    tstar[i] = median(xstar)
  }
  
  pl = sum(tstar<t_stat)/B
  pr=sum(tstar>t_stat)/B
  p = 2*min(pl,pr)
  parr[constindex] = p
  constindex = constindex+1
}

seqconst[parr>0.05]
plot(parr)

#p is always below 0.05 and therefore it does not stem from the exponential distribution. 
#Furthermore, the histogram resembles more of a bell-shaped curve rather than an exponential one

hist(data)
boxplot(tstar)
qqnorm(tstar); qqline(tstar)
