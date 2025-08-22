# Eksempler i R, 22. august 2025
########################################################################

# 1: Å regne sannsynligheter (diskret)

# X binomisk, 10 forsøk, suksess-sannsynlighet p = 0.25
# P(X = x)
?dbinom
dbinom(5,10,0.25)

# P(X ≤ x)
pbinom(5, 10, 0.25)

# Utregninger for mange x samtidig
xvec = c(0,1,2,3,4,5,6,7,8,9,10)  # eller xvec = seq(0,10)
xvec
dbinom(xvec,10,0.25)

# enkel plotting av punktsannsynligheter
plot(x = xvec, y = dbinom(xvec,10,0.25))


########################################################################
# 2: Å regne sannsynligheter (kontinuerlig)

# X normal med forventning 5 og varians 2
# f(x), tetthet
dnorm(3, 5, sqrt(2))

# P(X ≤ x)
pnorm(3, 5, sqrt(2))

# enkel plotting av tetthetsfunksjon
xvec = seq(0,10,0.00001)
plot(x = xvec, y = dnorm(xvec,5,sqrt(2)), type = "l")



########################################################################
# 3: Trekke fra en fordeling

# 20 trekninger fra en standard normalfordeling
zvec = rnorm(20)
zvec

# 20 trekninger fra en N(7.5, 1)-fordeling (TWIST)
x = rnorm(20, 7.5, 1)
x
mean(x)  # kjør disse linjene om igjen...


########################################################################
# 4: Simulere fordeling til estimatorer / observatorer (TWIST-eksempel)

# Simulere fordeling av gjennomsnittet (vi vet at svaret er N(7.5, 1/20))
xbar = c()
nsim = 10000 # antall simuleringer
for(j in 1:nsim){
  x = rnorm(20, 7.5, 1)
  xbar[j] = mean(x)
}

hist(xbar)  # her ser vi (simulert) fordeling til testobservatoren vår under H0


# Simulere fordeling (under H0) til sannsynlighetskvoten lambda
lambda = c()
nsim = 10000
for(j in 1:nsim){
  x = rnorm(20, 7.5, 1)
  upper = prod(dnorm(x,7.5,1))
  lower = prod(dnorm(x,mean(x),1))
  lambda[j] = upper/lower
}

hist(lambda, freq = FALSE)

# sammenligne med vår observasjon
x = c(10, 7, 6, 8, 6, 7, 6, 6, 6, 9, 7, 5, 5, 6, 8, 7, 5, 7, 7, 7)
mean(x)
upper = prod(dnorm(x,7.5,1))
lower = prod(dnorm(x,mean(x),1))
lambda_obs = upper/lower
lambda_obs
abline(v = lambda_obs, col = "red")

# kan estimere p-verdi:
sum(lambda<= lambda_obs)/nsim

# kan også estimere k
quantile(lambda,0.05)


# Ekstra:
# Simulere fordeling (under H0) til -2ln(lambda)

lnlambda = c()
nsim = 10000
for(j in 1:nsim){
  x = rnorm(20, 7.5, 1)
  upper = prod(dnorm(x,7.5,1))
  lower = prod(dnorm(x,mean(x),1))
  lnlambda[j] = -2*log(upper/lower)
}

hist(lnlambda, freq = FALSE, breaks = 30)
xnew = seq(0,20,0.001)
lines(x = xnew, y = dchisq(xnew, 1), col = "red")  

