#Example partially taken from
#https://cran.r-project.org/web/packages/rstanarm/vignettes/rstanarm.html

library(HSAUR3)
library(rstanarm)
data("womensrole", package = "HSAUR3")
womensrole_df<-data.frame(womensrole)

womensrole$total <- womensrole$agree + womensrole$disagree
womensrole_glm_1 <- glm(cbind(agree, disagree) ~ education + gender,
                        data = womensrole, family = binomial(link = "logit"))
round(coef(summary(womensrole_glm_1)), 3)

library(rstanarm)
womensrole_bglm_1 <- stan_glm(cbind(agree, disagree) ~ education + gender,
                              data = womensrole,
                              family = binomial(link = "logit"), 
                              prior = student_t(df = 7), 
                              prior_intercept = student_t(df = 7),
                              chains = 4, cores = 2, seed = 12345, iter=3000)
womensrole_bglm_1

summary(womensrole_bglm_1)

#Priors
prior_summary(womensrole_bglm_1, digits = 2)
priors<-prior_summary(womensrole_bglm_1, digits = 2)
names(priors)
priors$prior$scale
priors$prior$adjusted_scale


ci95 <- posterior_interval(womensrole_bglm_1, prob = 0.95, pars = "education")
round(ci95, 2)

cbind(Median = coef(womensrole_bglm_1), MAD_SD = se(womensrole_bglm_1))

#Traceplots
library(bayesplot)
library(ggplot2)
color_scheme_set("mix-blue-pink")
plot(womensrole_bglm_1, plotfun = "trace") + ggtitle("Traceplots")

#ACF
#
plot(womensrole_bglm_1, plotfun = "acf") + ggtitle("Autocorrelation Plots")

#we are plotting the 50% uncertainty interval (thick horizontal lines) and the 90% uncertainty interval (thin horizontal lines). In terms of interpretation, the 50% uncertainty interval identifies 
#where 50% of the marginal distribution lies for each parameter.
bayesplot_grid(
  plot(womensrole_bglm_1, plotfun = "intervals", prob = 0.5, prob_outer = 0.9, point_est = "median") + ggtitle("Marginal Posterior Parameter Estimates"),
  plot(womensrole_bglm_1, plotfun = "areas", prob = 0.5, prob_outer = 0.9, point_est = "median") + ggtitle("Marginal Posterior Parameter Estimates"),
  grid_args = list(ncol = 2)
)

#plot the histograms for each parameter (pooled across chains) and the empirical density of each parameter, respectively.
bayesplot_grid(
  plot(womensrole_bglm_1, plotfun = "hist") + ggtitle("Marginal Posterior Parameter Distributions"),
  plot(womensrole_bglm_1, plotfun = "dens_overlay",stat_bin(bins = 20)) + ggtitle("Marginal Posterior Parameter Distributions"), 
  grid_args = list(nrow = 2)
)

launch_shinystan(womensrole_bglm_1)

help('prior_summary.stanreg')

### Logistic regression
head(wells)
wells$dist100 <- wells$dist / 100
fit3 <- stan_glm(
  switch ~ dist100 + arsenic, 
  data = wells, 
  family = binomial(link = "logit"), 
  prior_intercept = normal(0, 10),
  QR = TRUE,
  chains = 2, iter = 200 # for speed of example only
)
print(fit3)
prior_summary(fit3)

#with MPG
library(rstanarm)
mtcars$log_mpg <- log(mtcars$mpg)
fit1 <- stan_glm(mpg ~ wt, data = mtcars)
fit2 <- stan_glm(log_mpg ~ wt, data = mtcars)

y <- mtcars$mpg
yrep1 <- posterior_predict(fit1, draws = 50)
yrep2 <- posterior_predict(fit2, fun = exp, draws = 50)

color_scheme_set("blue")
ppc1 <- ppc_dens_overlay(y, yrep1)
ppc1
ppc1 + yaxis_text()

color_scheme_set("red")
ppc2 <- ppc_dens_overlay(y, yrep2)
bayesplot_grid(ppc1, ppc2)

# make sure the plots use the same limits for the axes
bayesplot_grid(ppc1, ppc2, xlim = c(-5, 60), ylim = c(0, 0.2))

# remove the legends and add text
bayesplot_grid(ppc1, ppc2, xlim = c(-5, 60), ylim = c(0, 0.2),
               legends = FALSE, subtitles = rep("Predicted MPG", 2))