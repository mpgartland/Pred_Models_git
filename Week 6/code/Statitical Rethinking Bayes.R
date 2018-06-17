#code from Ch 3 of Statistical Rethinking by MCElreath
#Using Bayes Theorm to predict an outcome

#Blood test to predict being a vampire works 95% of the time
PrPV <- 0.95
# Pr(positve|mortal)
PrPM <- 0.01
#Percent of popultation that is a vampire
PrV <- 0.1

#Bayes Theorm
#Prob he is a vampire Pr(vampire|positive)
PrP <- PrPV*PrV + PrPM*(1-PrV)
( PrVP <- PrPV*PrV / PrP )

##################################
#Sampling 10,000 from Posterior Distribution
#simulation via grid approximation.
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

#generate samples for sampling grid and posterior distribution
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

#see sampling grid
plot( samples )

#See density function of sampling distrubtion. This is the posterior distribution
library(rethinking)
dens( samples )




## R code 3.6
# add up posterior probability where p < 0.5
sum( posterior[ p_grid < 0.5 ] )

## R code 3.7
sum( samples < 0.5 ) / 1e4

## R code 3.8
sum( samples > 0.5 & samples < 0.75 ) / 1e4

## R code 3.9
quantile( samples , 0.8 )

## R code 3.10
quantile( samples , c( 0.1 , 0.9 ) )

chainmode(samples)


##################
#Another example with a left skewed PD
## R code 3.11
p_grid1 <- seq( from=0 , to=1 , length.out=1000 )
prior1 <- rep(1,1000)
likelihood1 <- dbinom( 3 , size=3 , prob=p_grid1 )
posterior1 <- likelihood1 * prior1
posterior1 <- posterior1 / sum(posterior1)
samples1 <- sample( p_grid1 , size=1e4 , replace=TRUE , prob=posterior1 )

plot(samples1)

dens(samples1)

## R code 3.12
PI( samples1 , prob=0.5 )

## R code 3.13
#More robust CI
HPDI( samples1 , prob=0.5 )

## R code 3.14
#Find the max 
p_grid[ which.max(posterior1) ]

## R code 3.15
#Find the mode
chainmode( samples1 , adj=0.01 )

## R code 3.16
#Problems with the mean
mean( samples1 )
median( samples1 )

## R code 3.17
sum( posterior1*abs( 0.5 - p_grid1 ) )

## R code 3.18
#adding a loss function
loss <- sapply( p_grid1 , function(d) sum( posterior1*abs( d - p_grid1 ) ) )

## R code 3.19
p_grid1[ which.min(loss) ]

## R code 3.20
dbinom( 0:2 , size=2 , prob=0.7 )

## R code 3.21
rbinom( 1 , size=2 , prob=0.7 )

## R code 3.22
rbinom( 10 , size=2 , prob=0.7 )



