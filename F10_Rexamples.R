library(ggplot2)
library(matlib) 

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

# 95% konf int
confint(modell)


data = data.frame(x = x , y = y)
ggplot(data, aes(x = x, y = y)) + 
  geom_point() + 
  geom_smooth(method='lm')


predictions = predict(modell, newdata = data.frame(x=seq(1, 3, length.out = 100)), interval = "prediction")
predictions_df = as.data.frame(predictions)
predictions_df$x = seq(1, 3, length.out = 100)

ggplot(data, aes(x=x)) + 
  geom_point(aes(y = y)) + 
  geom_smooth(method='lm',aes(y = y)) + 
  geom_ribbon(data = predictions_df, aes(ymin = lwr, ymax = upr), fill = "skyblue", alpha = 0.3)



# lin alg


# matrise med kovariater (including intercept):
X = cbind(rep(1,9),x)
y

# projeksjonsmatrise 
Proj = X%*%inv(t(X)%*%X)%*%t(X)

# projeksjon av y ned i col(X)
plot( x= x, y = y)
points(x = x, y = Proj%*%y, pch = 16, col = "blue")

# løsning av Xbeta = Py:
betahat = inv(t(X)%*%X)%*%t(X)%*%y
betahat

# husk tilpasset regresjonsmodell med lm()
modell
