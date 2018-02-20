data = scan("klm.txt")
data

#######################
# POINT 1
#######################

par(mfrow=c(1,2))
hist(data, prob=T)
qqnorm(data)
#distribution of data is far away from normal - we cannot use t.test (lecture 3 - slide 13)
#we will use sign test

# m0 = 31 - expected median duration
m0 = 31

#number of successes - we expect that duration is less or equal to m0
ns = sum(data <= m0)

#number of trials
nt = length(data)

binom.test(ns,nt,p=0.5) # p=0.5 - we consider median duration (N/2)
#conclusion: p < 0.05 - H0 is rejected


#######################
# POINT 2
#######################

ns_2 = sum(data > 72); # amount of deliveries that exceeded 72 days

binom.test(ns_2,nt,p=0.1) # p=0.1 - we consider that at most 10% of data is successfull
#conclusion: p < 0.05 - H0 is rejected
