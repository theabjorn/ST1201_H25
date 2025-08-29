# Sannsynlighetskvotetest for varians

# H0:
sigma0squared = 4; sigma0 = sqrt(sigma0squared)

# Fordeling av lambda under H0
lambda = c()
nsim = 100000
n = 10; mu = 0; sigma = sigma0
for(j in 1:nsim){
  x = rnorm(n, mu, sigma)
  sigmahatsquared = sum((x-mean(x))^2)/n
  
  lambda[j] = (sigmahatsquared / sigma0squared)^(n/2)*exp(-(n/2)*((sigmahatsquared / sigma0squared)-1))
}
hist(lambda, freq = FALSE, breaks = 20)

# kritisk verdi
kapprox = quantile(lambda, 0.05)
kapprox


# Fordeling av lambda under H1

lambda1 = c()
nsim = 100000
sigma = sqrt(8)
for(j in 1:nsim){
  x = rnorm(n, mu, sigma)
  sigmahatsquared = sum((x-mean(x))^2)/n
  
  lambda1[j] = (sigmahatsquared / sigma0squared)^(n/2)*exp(-(n/2)*((sigmahatsquared / sigma0squared)-1))
}
hist(lambda1, freq = FALSE, breaks = 20)

# teststyrke
sum(lambda1 < kapprox)/nsim








# Sannsynlighetskvotetest


# Hva skulle k*, a* og b* vært?

# Fordeling under H0
lambdastar = c()
nsim = 100000
n = 10; mu = 0; sigma = sqrt(4)
for(j in 1:nsim){
  x = rnorm(n, mu, sigma)
  sigmahatsquared = sum((x-mean(x))^2)/n
  s2 = var(x)
  u = (n-1)*s2/sigma0squared
  lambdastar[j] = (u/n)*exp(-u/n)
}
hist(lambdastar)
kstar = quantile(lambdastar, 0.05)
kstar

# kapprox^(2/n)*exp(-1)


uvec = seq(0,100,0.01)
fn_u = (uvec/n)*exp(-uvec/n)
plot(x = uvec, y = fn_u, type = "l", ylab = "(u/n)exp(-u/n)", xlab = "u")

abline(h = kstar, col = "red")
# sett på kritiske verdier fra kji-kvadrat
abline(v = qchisq(1-(0.05/2), n-1, lower.tail = FALSE), col = "blue") # ser at de ikke er helt "riktig"
abline(v = qchisq(0.05/2, n-1, lower.tail = FALSE), col = "blue")

