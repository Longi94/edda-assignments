# example
m = 30
n = 30
mu = 180
nu = 175
sd = 10
x = rnorm(m,mu,sd)
y = rnorm(n,nu,sd)
t.test(x,y,var.equal=TRUE)
t.test(x,y,var.equal=TRUE)[[3]]

B = 1000
p = numeric(B)
for (b in 1:B) {
  x = rnorm(m,mu,sd)
  y = rnorm(n,nu,sd)
  p[b] = t.test(x,y,var.equal=TRUE)[[3]]
}

power = mean(p < 0.05)
# power around 0.488

################################
# excercise
################################
# point 1
################################
m = 30
n = 30
mu = 180
nu = 180
sd = 10
p = numeric(B)
for (b in 1:B) {
  x = rnorm(m,mu,sd)
  y = rnorm(n,nu,sd)
  p[b] = t.test(x,y,var.equal=TRUE)[[3]]
}

#power for nu=180, less than 5%
power_nu180_05_v1 = mean(p < 0.05) # power_nu180_05 around 0.059 - very low probability that t-test rejects null hypothesis - makes sense since men and women have equal mean height

#power for nu=180, less than 5% 
power_nu180_10_v1 = mean(p < 0.1) # power_nu180_10 around 0.100 - 2x more than previous assumption, no idea

hist(p) # histogram is similar to uniform distribution

################################
# point 2
################################
m = 30
n = 30
mu = 180
nu = 180
sd = 1
p = numeric(B)
for (b in 1:B) {
  x = rnorm(m,mu,sd)
  y = rnorm(n,nu,sd)
  p[b] = t.test(x,y,var.equal=TRUE)[[3]]
}

#power for nu=180, less than 5%
power_nu180_05_v2 = mean(p < 0.05) # did not significally changed in comparison to v1

#power for nu=180, less than 5% 
power_nu180_10_v2 = mean(p < 0.1) # did not significally changed in comparison to v1

# I think that because both populations are the same height, change of sd doesn't change the power since populations do not differ - null hypothesis has no chance to be rejected

hist(p) # histogram is similar to uniform distribution

################################
# point 3
################################
m = 30
n = 30
mu = 180
nu = 175
sd = 6
p = numeric(B)
for (b in 1:B) {
  x = rnorm(m,mu,sd)
  y = rnorm(n,nu,sd)
  p[b] = t.test(x,y,var.equal=TRUE)[[3]]
}

#power for nu=180, less than 5%
power_nu180_05_v3 = mean(p < 0.05) # power around 0.9

#power for nu=180, less than 5% 
power_nu180_10_v3 = mean(p < 0.1) # power around 0.95

# comment: sd is quite low, populations have different mean height => probability of null hypothesis rejection is high
# comment2: (test: I have inceased sd to 100) when we increase sd (standard deviation) - plot of normal distribution is much wider so population characteristics cross with each other => hypothesis is less likely to be rejected

hist(p) # histogram is similar to expotential distribution?
