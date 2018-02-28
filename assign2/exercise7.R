par(mfrow=c(1,2))
dogs = read.csv(file = "dogs.txt", header = TRUE, sep = " ")

attach(dogs)

# 1.

testnorm_isofluorane = rnorm(length(isofluorane))
testnorm_halothane = rnorm(length(halothane))
testnorm_cyclopropane = rnorm(length(cyclopropane))

qqnorm(isofluorane, main="Original Isofluorane data"); qqline(isofluorane)
qqnorm(testnorm_isofluorane, main="Generated Isofluorane data"); qqline(testnorm_isofluorane)

qqnorm(halothane, main="Original Halothane data"); qqline(halothane)
qqnorm(testnorm_halothane, main="Generated Halothane data"); qqline(testnorm_halothane)

qqnorm(cyclopropane, main="Original Cyclopropane data"); qqline(cyclopropane)
qqnorm(testnorm_cyclopropane, main="Generated Cyclopropane data"); qqline(testnorm_cyclopropane)

boxplot(dogs)

# Seems reasonable for the last one, second one is questionable, first is not a normal population

# 2.
# use permutation tests to see the difference between the outcomes
mystat = function (x, y) { mean(x - y) }
B = 1000

# isofluorane and halothane
tstar = numeric(B)

for (i in 1:B) {
  constar = t(apply(cbind(isofluorane, halothane), 1, sample))
  tstar[i] = mystat(constar[,1], constar[,2])
}
myt1 = mystat(isofluorane, halothane)

hist(tstar)
pl1 = sum(tstar < myt1) / B
pr1 = sum(tstar > myt1) / B
p = 2 * min(pl1, pr1)
p # 0.696 accept the null hypo, same concetration that isofluorane and halothane has the same concentration

# cyclopropane and halothane
tstar = numeric(B)

for (i in 1:B) {
  constar = t(apply(cbind(cyclopropane, halothane), 1, sample))
  tstar[i] = mystat(constar[,1], constar[,2])
}
myt2 = mystat(cyclopropane, halothane)

hist(tstar)
pl2 = sum(tstar < myt2) / B
pr2 = sum(tstar > myt2) / B
p = 2 * min(pl2, pr2)
p # 0.058 there is a big difference between the concetrations

# isofluorane and cyclopropane
tstar = numeric(B)

for (i in 1:B) {
  constar = t(apply(cbind(isofluorane, cyclopropane), 1, sample))
  tstar[i] = mystat(constar[,1], constar[,2])
}
myt3 = mystat(isofluorane, cyclopropane)

hist(tstar)
pl3 = sum(tstar < myt3) / B
pr3 = sum(tstar > myt3) / B
p = 2 * min(pl3, pr3)
p # 0.48 there is a big difference between the concetrations

# estimated concentration values, this supports the above p-values
mean(isofluorane) # 0.434
mean(halothane) # 0.469
mean(cyclopropane) # 0.853

# 3.
dogframe = data.frame(drugs = as.vector(as.matrix(dogs)), groups = as.factor(rep(1:3, each = 10)))
attach(dogframe)
test= kruskal.test(drugs, groups)
summary(test)
# > kruskal.test(drugs, groups)
# 
# Kruskal-Wallis rank sum test
# 
# data:  drugs and groups
# Kruskal-Wallis chi-squared = 5.6442, df = 2, p-value = 0.05948  

# quite close to 0.05, but still accept the null hypo

# The difference is that kruskal tests wether all three groups are from the same population or not,
# while the t-test only tests 2, so 2 populations can be the same, but different from the third
# The two results are compatibel with each other, 3 populations are not the same, 2 of the 3 are the
# same, but no the third (cyclopropane)

detach(dogs)
detach(dogframe)
