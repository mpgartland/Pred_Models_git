
## R code 8.2
library(rethinking)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#Set Data to me mapped
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]

## R code 8.3
#Create a function for the PD from this data set given
#the below regression 
m8.1 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ) ,
  data=dd )

#Review Model
precis(m8.1)

plot(m8.1)

## R code 8.4
#Simplify the data set
dd.trim <- dd[ , c("log_gdp","rugged","cont_africa") ]
str(dd.trim)

## R code 8.5
#Remap (to STAN) the regression function 
#sigma to a uniform prior
#using a Hamiltonian MCMC to sample from the PD
m8.1stan <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dcauchy(0,2)
  ) ,
  data=dd.trim,iter=2000,warmup = 1000 )

#View MCMC model via STAN

print(m8.1stan)

## R code 8.6
#Updated Regression output
precis(m8.1stan)

## R code 8.7
#Using 4 Chains
m8.1stan_4chains <- map2stan( m8.1stan , chains=4 , cores=4 )
precis(m8.1stan_4chains)

## R code 8.8
#extact sampled values
post <- extract.samples( m8.1stan )
str(post)


## R code 8.9
pairs(post)

#PD of each parameter
dens(post$a)
dens(post$bR)

## R code 8.10
pairs(m8.1stan)

## R code 8.11
show(m8.1stan)

## R code 8.12
plot(m8.1stan)







#########################
#Additional code)
#########################
## R code 8.13
y <- c(-1,1)
m8.2 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- alpha
  ) ,
  data=list(y=y) , start=list(alpha=0,sigma=1) ,
  chains=2 , iter=4000 , warmup=1000 )

## R code 8.14
precis(m8.2)

## R code 8.15
m8.3 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- alpha ,
    alpha ~ dnorm( 1 , 10 ) ,
    sigma ~ dcauchy( 0 , 1 )
  ) ,
  data=list(y=y) , start=list(alpha=0,sigma=1) ,
  chains=2 , iter=4000 , warmup=1000 )
precis(m8.3)

## R code 8.16
y <- rcauchy(1e4,0,5)
mu <- sapply( 1:length(y) , function(i) sum(y[1:i])/i )
plot(mu,type="l")

## R code 8.17
y <- rnorm( 100 , mean=0 , sd=1 )

## R code 8.18
m8.4 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a1 + a2 ,
    sigma ~ dcauchy( 0 , 1 )
  ) ,
  data=list(y=y) , start=list(a1=0,a2=0,sigma=1) ,
  chains=2 , iter=4000 , warmup=1000 )
precis(m8.4)

## R code 8.19
m8.5 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a1 + a2 ,
    a1 ~ dnorm( 0 , 10 ) ,
    a2 ~ dnorm( 0 , 10 ) ,
    sigma ~ dcauchy( 0 , 1 )
  ) ,
  data=list(y=y) , start=list(a1=0,a2=0,sigma=1) ,
  chains=2 , iter=4000 , warmup=1000 )
precis(m8.5)

## R code 8.20
mp <- map2stan(
  alist(
    a ~ dnorm(0,1),
    b ~ dcauchy(0,1)
  ),
  data=list(y=1),
  start=list(a=0,b=0),
  iter=1e4, warmup=100 , WAIC=FALSE )

## R code 8.21
N <- 100                          # number of individuals
height <- rnorm(N,10,2)           # sim total height of each
leg_prop <- runif(N,0.4,0.5)      # leg as proportion of height
leg_left <- leg_prop*height +     # sim left leg as proportion + error
  rnorm( N , 0 , 0.02 )
leg_right <- leg_prop*height +    # sim right leg as proportion + error
  rnorm( N , 0 , 0.02 )
# combine into data frame
d <- data.frame(height,leg_left,leg_right)

## R code 8.22
m5.8s <- map2stan(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) ,
    sigma ~ dcauchy( 0 , 1 )
  ) ,
  data=d, chains=4,
  start=list(a=10,bl=0,br=0,sigma=1) )

## R code 8.23
m5.8s2 <- map2stan(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) & T[0,] ,
    sigma ~ dcauchy( 0 , 1 )
  ) ,
  data=d, chains=4,
  start=list(a=10,bl=0,br=0,sigma=1) )
