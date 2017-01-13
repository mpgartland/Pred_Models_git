library(mboost)
library(caret)

#Generalized Linear Model BOOSTED
cars.gb <- glmboost(dist ~ speed, data = cars,
                    control = boost_control(mstop = 2000),
                    center = FALSE)
print(cars.gb)

AIC(cars.gb, method = "corrected")


### coefficients should coincide
cf <- coef(cars.gb, off2int = TRUE)     ## add offset to intercept
coef(cars.gb) + c(cars.gb$offset, 0)    ## add offset to intercept (by hand)
signif(cf, 3)
signif(coef(lm(dist ~ speed, data = cars)), 3)
## almost converged. With higher mstop the results get even better

### now we center the design matrix for
### much quicker "convergence"
cars.gb_centered <- glmboost(dist ~ speed, data = cars,
                             control = boost_control(mstop = 2000),
                             center = TRUE)

AIC(cars.gb_centered, method = "corrected")



#GAM with BOOST
## plot coefficient paths oth glmboost
par(mfrow=c(1,2), mai = par("mai") * c(1, 1, 1, 2.5))
plot(cars.gb, main="without centering")
plot(cars.gb_centered, main="with centering")

cars.gb <- gamboost(dist ~ speed, data = cars, dfbase = 4,
                    control = boost_control(mstop = 50))

print(cars.gb)
AIC(cars.gb, method = "corrected")

### plot fit for mstop = 1, ..., 50
plot(dist ~ speed, data = cars)
tmp <- sapply(1:mstop(AIC(cars.gb)), function(i)
  lines(cars$speed, predict(cars.gb[i]), col = "red"))
lines(cars$speed, predict(smooth.spline(cars$speed, cars$dist),
                          cars$speed)$y, col = "green")

