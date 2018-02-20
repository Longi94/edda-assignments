dogs = read.csv(file = "dogs.txt", header = TRUE, sep = " ")

attach(dogs)

# 1.
boxplot(dogs)

qqnorm(isofluorane)
qqnorm(halothane)
qqnorm(cyclopropane)

# Seems reasonable for the last one, second one is questionable, first is not a normal population

# 2.
# not sure, I think they mean that we should just do a two sample t test on each pair of groups
t.test(isofluorane,halothane,var.equal=TRUE)[[3]] # 0.7327132 accept null hypo
t.test(cyclopropane,halothane,var.equal=TRUE)[[3]] # 0.0240889 reject null hypo
t.test(isofluorane,cyclopropane,var.equal=TRUE)[[3]] # 0.01827111 reject null hypo

# estimated concentration values, this supports the above p-values
mean(isofluorane) # 0.434
mean(halothane) # 0.469
mean(cyclopropane) # 0.853

# 3.
dogframe = data.frame(drugs = as.vector(as.matrix(dogs)), groups = as.factor(rep(1:3, each = 10)))
attach(dogframe)
kruskal.test(drugs, groups)

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
