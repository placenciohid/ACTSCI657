# Consider data set HealthData.txt. The data summarize the emergency room utilization for a random sample of U.S. population. The variable of interest is the annual expenditure of ER utilization, i.e. erexp. Take a subset of the data that contain observations with positive expenditure.

# In this exercise, we are going to evaluate the probabilistic calibration for various heavy-tailed regressions. Split the data into two sets, training and test. Randomly selected n=3,000 observations for the training data, and use the rest for the test. Use set.seed(123) for replication purposes.

# read data
rm(list=ls())
dat <- read.table(file='HealthData.txt', header = TRUE, sep = "", dec = ".")
dat.pos <- subset(dat,erexp>0)

set.seed(123)
index <- sample(1:nrow(dat.pos),3000)
dat.train <- dat.pos[index,]
dat.test <- dat.pos[-index,]

# 1. The first modeler’s subject predictive distribution is a gamma model based on information set that contains predictors on demographics (age,female,married, race), health related predictors (limitation, chronic, smoke), social economic factors (edu, log of income, region). Fit this model using training data, and calculate the PITs for observations in the test data. Show the histogram of the PITs (use function hist(,prob=T)).
library(MASS)
library(ggplot2)

# Fit a gamma GLM model with log link
glm_gamma <- glm(erexp ~ age + female + married + race + limitation + chronic + smoke + edu + log(income) + region,
                 data = dat.train, family = Gamma(link = "log"))

# Predict the test dataset
preds_test <- predict(glm_gamma, newdata = dat.test, type = "response")

# Compute the gamma shape parameter
gamma_shape <- summary(glm_gamma)$dispersion

# Calculate the PITs
pits_test <- pgamma(dat.test$erexp, shape = gamma_shape, scale = preds_test/gamma_shape)

# Plot the histogram of PITs
hist(pits_test, prob = TRUE, main = "Histogram of PITs for Gamma Model")

# 2. The second modeler’s subject predictive distribution is a Weibull hazard model based on information set that contains predictors on demographics (age,female,married, race), health related predictors (limitation, chronic, smoke), social economic factors (edu, log of income, region). Fit this model using training data, and calculate the PITs for observations in the test data. Show the histogram of the PITs (use function hist(,prob=T)).

library(survival)

# Fit a Weibull hazard model
weibull_fit <- survreg(Surv(erexp) ~ age + female + married + race + limitation + chronic + smoke + edu + log(income) + region,
                       data = dat.train, dist = "weibull")

# Predict the test dataset
preds_test <- predict(weibull_fit, newdata = dat.test, type = "response")

# Calculate the PITs
pits_test <- pweibull(dat.test$erexp, shape = weibull_fit$scale, scale = preds_test/weibull_fit$scale)

# Plot the histogram of PITs
hist(pits_test, prob = TRUE, main = "Histogram of PITs for Weibull Model")

# 3. The second modeler’s subject predictive distribution is a Cox hazard model based on information set that contains predictors on demographics (age,female,married, race), health related predictors (limitation, chronic, smoke), social economic factors (edu, log of income, region). Fit this model using training data, and calculate the PITs for observations in the test data. Show the histogram of the PITs (use function hist(,prob=T)).

# fit model
cox.mod <- coxph(Surv(erexp) ~ age + female + married + race + limitation + chronic + smoke + edu + log(income) + region, data = dat.train)

# Calculate linear predictor for test data
test.cox <- predict(cox.mod, newdata = dat.test, type = "risk")

# Calculate the PITs
pits_test <- pexp(dat.test$erexp, rate = test.cox)

# Plot the histogram of PITs
hist(pits_test, prob = TRUE, main = "Histogram of PITs")
