# kontigenstabell og hypotesetest

# simulerer uavhengige A-er og B-er (H0 sann)
# og studerer fordelingen til observatoren D


r = 3
k = 4
pvec = c(0.2,0.3,0.5)
qvec = rep(0.25, 4)
n = 100

Apartition = paste0("A",c(1,2,3))
Apartition
Bpartition = paste0("B",c(1,2,3,4))

# En simulering av n forsøk:
avec = sample(Apartition,size = n, prob = pvec, replace = TRUE)
bvec = sample(Bpartition,size = n, prob = qvec, replace = TRUE)
Rvec = table(avec) # radsummer
Kvec = table(bvec) # kolonnesummer
outer(Rvec, Kvec, "*")/n # estimert forventet antall, under H0
table(avec,bvec)  # simulerte utfall (under H0)


nsim = 10000
d_obs = c()
for(s in 1:nsim){
  avec = sample(Apartition,size = n, prob = pvec, replace = TRUE)
  bvec = sample(Bpartition,size = n, prob = qvec, replace = TRUE)
  Rvec = table(avec)
  Kvec = table(bvec)
  Evec = c(outer(Rvec, Kvec, "*")/n) # RiKj/n som vektor
  Xvec = c(table(avec,bvec)) # observasjoner (tellinger) som vector
  d_obs[s] = sum((Xvec - Evec)^2/Evec)
}

dof = (3-1)*(4-1)

hist(d_obs, freq = FALSE)
lines(x = seq(0,25,0.001), y = dchisq(seq(0,25,0.001), dof))








# oppgave 10.5.10

tabGrades = 
  matrix(c(115,91,65,42,124,148,128,69,107,85,121,43,37,35,37,16),
         ncol = 4, nrow = 4,byrow = TRUE)

chisq.test(tabGrades, correct = FALSE)
?chisq.test

