
# høydehus:
xvec = c(16.3, 15.4, 14.2, 13.7, 14.8, 12.9)
mean(xvec) # bar(x)
var(xvec)  # s^2
sd(xvec)   # s

# ikke høydehus:
yvec = c(15.0, 14.3, 14.8, 13.2, 12.2, 13.1, 12.8)
mean(yvec)
var(yvec)
sd(yvec)

 # differanse i snitt
mean(xvec)-mean(yvec)

# s-interpolert 
s_p = sqrt( ((6-1)*var(xvec) + (7-1)*var(yvec))/(6+7-2) )
s_p

# 99% konfint
t_kritisk = qt(0.01/2, 11, lower.tail = FALSE)
mean(xvec)-mean(yvec) - t_kritisk*s_p*sqrt(1/6 + 1/7)
mean(xvec)-mean(yvec) + t_kritisk*s_p*sqrt(1/6 + 1/7)

# to-utvalgs t-test og konfidensintervall, lik varians
?t.test
t.test(xvec,yvec, var.equal = TRUE, conf.level = 0.99)


# to-utvalgs t-test og konfidensintervall, ulik varians
t.test(xvec,yvec, var.equal = FALSE, conf.level = 0.99)
