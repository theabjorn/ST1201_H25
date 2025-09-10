

# bags of candy
obs = c(100,47,62,53,38)
expected = 300*c(0.26,0.14, 0.24, 0.20, 0.16)
d_obs = sum( (obs-expected)^2/expected )

dof = 5-1
d_obs

x = seq(0,20,0.0001)
plot(x = x , y = dchisq(x,dof), type = "l")
abline(v = qchisq(0.05, dof, lower.tail = FALSE), col = "red")
abline(v = d_obs, col = "blue", lty = 2)
















# kontigenstabell

n = 350000
n_nord = 100000
n_sør = 250000
n_borg = 128300
n_ikkeborg = 221700

# forventede tall ved uavhengighet
nord_e = c( n_nord*n_borg/n,  n_nord*n_ikkeborg/n )
sør_e = c( n_sør*n_borg/n,  n_sør*n_ikkeborg/n )
tab_e = rbind(nord_e, sør_e)
tab_e





# observerte stemmetall nord- og sør-trøndelag (sånn omtrent)
nord = c(32800, 67200) # borgerlig, ikke-borg
sør = c(95500, 154500)
tab_obs = rbind(nord,sør)
tab_obs



# d_obs
alle_obs = c(nord, sør)
alle_e = c(nord_e, sør_e)
d_obs = sum(  (alle_obs-alle_e)^2/alle_e )
d_obs

chisq.test(tab_obs)


x = seq(0,5,0.0001)
plot(x = x , y = dchisq(x,1), type = "l", ylim = c(0,4))
abline(v = qchisq(0.05, 1, lower.tail = FALSE), col = "red")
abline(v = d_obs, col = "blue", lty = 2)


