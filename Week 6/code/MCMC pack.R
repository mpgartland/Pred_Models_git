install.packages("MCMCpack")

data(birthwt)
model1 <- MCMCregress(bwt~age+lwt+as.factor(race) + smoke + ht,
                      data=birthwt, b0=c(2700, 0, 0, -500, -500,
                                         -500, -500),
                      B0=c(1e-6, .01, .01, 1.6e-5, 1.6e-5, 1.6e-5,
                           1.6e-5), c0=10, d0=4500000,
                      marginal.likelihood="Chib95", mcmc=10000)
model2 <- MCMCregress(bwt~age+lwt+as.factor(race) + smoke,
                      data=birthwt, b0=c(2700, 0, 0, -500, -500,
                                         -500),
                      B0=c(1e-6, .01, .01, 1.6e-5, 1.6e-5, 1.6e-5),
                      c0=10, d0=4500000,
                      marginal.likelihood="Chib95", mcmc=10000)
model3 <- MCMCregress(bwt~as.factor(race) + smoke + ht,
                      data=birthwt, b0=c(2700, -500, -500,
                                         -500, -500),
                      B0=c(1e-6, 1.6e-5, 1.6e-5, 1.6e-5,
                           1.6e-5), c0=10, d0=4500000,
                      marginal.likelihood="Chib95", mcmc=10000)
BF <- BayesFactor(model1, model2, model3)

plot(model1)
print(BF)