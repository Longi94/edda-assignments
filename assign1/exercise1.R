# load data from file
load(file="assign1.RData")

# analysing vector x1
par(mfrow=c(2,2))

hist(x1)
qqnorm(x1); qqline(x1)

x1_testNorm = rnorm(length(x1))
hist(x1_testNorm)
qqnorm(x1_testNorm); qqline(x1_testNorm)
# KOKAR my opinion: x1 is sampled from normal distribution - quantiles generally keep in line, we have to remember that we have low amount of samples so such errors are acceptable

# analysing vector x2
par(mfrow=c(2,2))

hist(x2)
qqnorm(x2); qqline(x2)

x2_testNorm = rnorm(length(x2))
hist(x2_testNorm)
qqnorm(x2_testNorm); qqline(x2_testNorm)
# KOKAR my opinion: x2 is not sampled from normal distribution - quantiles of the middle part keep in line (large amount of samples), but tails have significant tendency to follow another distribution

# analysing vector x3
par(mfrow=c(2,2))

hist(x3)
qqnorm(x3); qqline(x3)

x3_testNorm = rnorm(length(x3))
hist(x3_testNorm)
qqnorm(x3_testNorm); qqline(x3_testNorm)
# KOKAR my opinion: x3 is sampled from normal distribution - quantiles of the middle part keep exactly in line (large amount of samples), tails are aound line, qqPlot is very similar to test

# analysing vector x4
par(mfrow=c(2,2))

hist(x4)
qqnorm(x4); qqline(x4)

x4_testNorm = rnorm(length(x4))
hist(x4_testNorm)
qqnorm(x4_testNorm); qqline(x4_testNorm)
# KOKAR my opinion: x4 is sampled from normal distribution - quantiles keep in line even with such a low amount of samples (30)

# analysing vector x5
par(mfrow=c(2,2))

hist(x5)
qqnorm(x5); qqline(x5)

x5_testNorm = rnorm(length(x5))
hist(x5_testNorm)
qqnorm(x5_testNorm); qqline(x5_testNorm)
# KOKAR my opinion: x5 is not sampled from normal distribution - low and high quantiles have regular tendency to go away from straight line