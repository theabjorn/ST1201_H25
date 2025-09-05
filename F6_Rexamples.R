
data(iris)  # laster inn datasettet
summary(iris)

sepal_length_setosa = iris$Sepal.Length[iris$Species == "setosa"]
sepal_length_virginica = iris$Sepal.Length[iris$Species == "virginica"]

# enkel plotting
hist(sepal_length_setosa)
hist(sepal_length_virginica)

# litt mer avansert plotting
library(ggplot2)
ggplot(data = iris[iris$Species != "versicolor",], aes(x = Sepal.Length, fill = Species)) + 
  geom_histogram(bins = 10)

ggplot(data = iris[iris$Species != "versicolor",], aes(x = Sepal.Length, fill = Species)) + 
  geom_density()





# test forskjell i varianser
?var.test

var.test(sepal_length_setosa, sepal_length_virginica)

# kritiske verdier
qf(0.975, 49, 49, lower.tail = FALSE)
qf(0.025, 49, 49, lower.tail = FALSE)

Fobs = var(sepal_length_setosa)/var(sepal_length_virginica)
Fobs

# konfidensintervall
Fobs*qf(0.975, 49, 49, lower.tail = FALSE) # husk å holde styr på m og n (like her)
Fobs*qf(0.025, 49, 49, lower.tail = FALSE)





# Problemer med å bruke F-test for å avgjøre hvilken t-test vi skal bruke
# Like forventningsverdier, ulike varianser
nsim = 100000
p_val = c()
n = 6; m = 7 # som i høydehus-dataene fra forelesning 5
f_critical_l = qf(0.975, n-1, m-1, lower.tail = FALSE)
f_critical_u = qf(0.025, n-1, m-1, lower.tail = FALSE)

for(i in 1:nsim){
  xvec = rnorm(n,15,2)
  yvec = rnorm(m,15,1)
  sx2 = var(xvec)
  sy2 = var(yvec)
  fobs = sx2/sy2
  if((fobs < f_critical_u) & (fobs > f_critical_l)){ # ikke forkast, anta lik varians
    p_val[i] = t.test(xvec,yvec, var.equal = TRUE, conf.level = 0.05)$p.value
  }else{
    p_val[i] = t.test(xvec,yvec, var.equal = FALSE, conf.level = 0.05)$p.value
  }
}

mean(p_val < 0.05, na.rm = TRUE)
