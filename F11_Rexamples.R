# Enkel lineær regresjon, simulert eksempel
beta0 = 2
beta1 = -1
sigma2 = 1

# bestemmer 'instillinger' for x i et eksperiment
x = c(rep(1,50),rep(2,50),rep(3,50))
# genererer Y
y = rnorm(length(x), beta0 + beta1*x, sd = sqrt(sigma2))

# kryssplott av x og y
plot(x=x, y = y, pch = 16)

# estimere parametere i regresjonsmodell
modell = lm(y ~ x)
modell
summary(modell)

plot(modell)




# Enkel lineær regresjon, simulert eksempel med feil x
# genererer Y
y = rnorm(length(x), beta0 + beta1*(x^3), sd = sqrt(sigma2))  #NB!!

# estimere parametere i (feil) regresjonsmodell
modell2 = lm(y ~ x)
plot(modell2)

# estimere parametere i (riktig) regresjonsmodell
nyx = x^3
modell3 = lm(y ~ nyx)
plot(modell3)












# Iris



data("iris")
data = iris[iris$Species != "virginica",]
plot(x = data$Petal.Length, y = data$Petal.Width, pch = 16)

mod1 = lm(Petal.Width ~ Petal.Length, data = data)
summary(mod1)


plot(x = data$Petal.Length, y = data$Sepal.Length, pch = 16)
mod2 = lm(Sepal.Length ~ Petal.Length, data = data)
summary(mod2)



# sammenligne petal.width i to grupper

library(ggplot2)

ggplot(data = data, aes(x = Petal.Width, fill = Species)) + 
  geom_density()

t.test(data[data$Species=="setosa",]$Petal.Width, 
       data[data$Species=="versicolor",]$Petal.Width)


data$SpeciesNum = as.numeric(data$Species)-1  # 0: setosa, 1: versicolor
data[1:5,]
plot(y = data$Petal.Width, x = data$SpeciesNum)

mod3 = lm( Petal.Width ~ SpeciesNum, data = data)
summary(mod3)


# Petal.Width ~ Petal.Length i to grupper

plot(x = data$Petal.Length, y = data$Sepal.Length, pch = 16)
mod2 = lm(Sepal.Length ~ Petal.Length, data = data)
summary(mod2)
abline(a = mod2$coefficients[1], b = mod2$coefficients[2], col = "red")


mod2.2 = lm(Sepal.Length ~ SpeciesNum*Petal.Length, data = data)
summary(mod2.2)
plot(x = data$Petal.Length, y = data$Sepal.Length, pch = 16)
abline(a = mod2.2$coefficients[1], b = mod2.2$coefficients[3], col = "blue")
abline(a = mod2.2$coefficients[1] + mod2.2$coefficients[2], 
       b = mod2.2$coefficients[3] + mod2.2$coefficients[4], col = "green")




# lin alg
library(matlib)

# matrise med kovariater (including intercept):
X = cbind(rep(1,100),data$SpeciesNum, data$Petal.Length, data$SpeciesNum*data$Petal.Length)
X[48:54,]
y = data$Sepal.Length

# projeksjonsmatrise 
Proj = X%*%inv(t(X)%*%X)%*%t(X)

# løsning av Xbeta = Py:
betahat = inv(t(X)%*%X)%*%t(X)%*%y
betahat

mod2.2$coefficients

