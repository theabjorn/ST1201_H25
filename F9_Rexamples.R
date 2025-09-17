library(ggplot2)


data("iris")
plot(x = iris$Petal.Length, y = iris$Petal.Width, pch = 16)












# Enkel lineær regresjon, simulert eksempel
beta0 = 2
beta1 = -1
sigma2 = 0.5

# bestemmer 'instillinger' for x i et eksperiment
x = c(1,1,1,2,2,2,3,3,3)
# genererer Y
y = rnorm(length(x), beta0 + beta1*x, sd = sqrt(sigma2))

# kryssplott av x og y
plot(x=x, y = y, pch = 16)

# estimere parametere i regresjonsmodell
modell = lm(y ~ x)
modell
summary(modell)

ssx = sum((x-mean(x))^2)
ssxy = sum((x-mean(x))*(y-mean(y)))
beta1hat = ssxy/ssx
beta1hat

s2 = sum(modell$residuals^2)/(length(y)-2)
s2

sqrt(s2/ssx)

# 95% konf int
beta1hat - sqrt(s2/ssx)*qt(0.05/2, 7, lower.tail = FALSE)
beta1hat + sqrt(s2/ssx)*qt(0.05/2, 7, lower.tail = FALSE)

# t-test
t_obs = beta1hat/sqrt(s2/ssx)






 

data = data.frame(x = x , y = y)
ggplot(data, aes(x = x, y = y)) + 
  geom_point() + 
  geom_smooth(method='lm')



