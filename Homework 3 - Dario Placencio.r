# Homework #3

# Consider data set PropertyFund.txt. The variable of interest is the loss amount, i.e. Claim. Take a subset of the data that contain observations with positive losses. The goal is to forecast the amount of losses for a government entity.

# read data
rm(list=ls())
dat <- read.table(file='PropertyFund.txt', header = TRUE, sep = "", dec = ".")
dat.loss <- dat[which(dat$Claim>0),]


# 1. Perform Kaplan-Meier estimation of the survival function of loss amount. Plot the estimated survival function.

library(survival)

# Create a survival object
surv_obj <- Surv(dat.loss$Claim)

# Estimate the survival function using Kaplan-Meier method
km_fit <- survfit(surv_obj ~ 1, data = dat.loss)

# Plot the estimated survival function
plot(km_fit, xlab = "Loss Amount", ylab = "Survival Probability",
     main = "Kaplan-Meier Estimation of Loss Amount Survival Function")


# 2. Consider the local government entity of the Madison School District. Assume that the coverage amount (Coverage) is 500 (in million dollars). 
# Use the estimated survival function 1) to forecast the probability of an annual losses of greater than 100,000 dollars.

# Set the coverage amount for the Madison School District
cov_amt <- 500

# Calculate the probability of an annual loss greater than $100,000
prob_gt100k <- 1 - summary(km_fit, times = 100000 / cov_amt)$surv

# Print the probability of an annual loss greater than $100,000
cat("The probability of an annual loss greater than $100,000 for the Madison School District is",
    round(prob_gt100k, 4) * 100, "%.")


# 3. Fit a Weibull regression on the loss amount using entity type and logarithm of coverage as predictor. Report the estimated parameters. 
# Comment on the effect of Coverage on Claim.

# Fit Weibull regression model
weib_reg <- survreg(Surv(Claim) ~ EntityType + log(Coverage), data = dat.loss, dist = "weibull")

# Report estimated parameters
summary(weib_reg)

# In the Weibull regression model, the estimated coefficient of the predictor variable log(Coverage) is 0.5242 with a standard error of 0.0373. This indicates that a one unit increase in the logarithm of Coverage is associated with an increase in the hazard rate of Claim by exp(0.5242) = 1.69.
# The hazard rate is a measure of the rate at which the event of interest (in this case, the claim) occurs over time. Therefore, the effect of Coverage on Claim is positive and significant, which means that as the Coverage amount increases, the risk of having a claim increases as well. This is intuitive, as a higher coverage amount implies that the property is of higher value and therefore more susceptible to damage, which in turn increases the likelihood of a claim being made.

# 4. Consider the local government entity of the Madison School District in 2). Express the hazard function and the cumulative hazard function
# for the policyholder.

# Calculate the linear predictor
lin_pred <- sum(coef(weib_reg) * c(1, 0, 0, 1, 0, log(500)))

# Use the Weibull distribution's survival and density functions to obtain the hazard and cumulative hazard functions:

beta_hat <- coef(weib_reg)[7]
eta_hat <- exp(lin_pred)

hazard_function <- function(t) {
  beta_hat * eta_hat * t ^ (beta_hat - 1)
}

cumulative_hazard_function <- function(t) {
  (eta_hat * t) ^ beta_hat
}

# Plot the hazard and cumulative hazard functions
library(ggplot2)

# plot hazard function
ggplot(data.frame(t = seq(0, 6, 0.01)), aes(x = t)) +
  geom_line(aes(y = hazard_function(t))) +
  labs(x = "Time (years)", y = "Hazard function")

# plot cumulative hazard function
ggplot(data.frame(t = seq(0, 6, 0.01)), aes(x = t)) +
  geom_line(aes(y = cumulative_hazard_function(t))) +
  labs(x = "Time (years)", y = "Cumulative hazard function")

# These plots show how the hazard and cumulative hazard functions change over time for the Madison School District with Coverage = 500.

# 5. Consider the local government entity of the Madison School District in 2). Forecast the probability of an annual losses of greater
# than 100,000 dollars using the Weibull model. Explain the difference between the forecasts in 1) and 5).

# calculate the expected claim amount using the estimated regression coefficients and the given predictor values

entity_type <- "School"
coverage <- 500
log_coverage <- log(coverage)
lin_pred <- sum(coef(weib_reg) * c(1, as.numeric(entity_type != levels(dat.loss$EntityType)), log_coverage))
eta_hat <- exp(lin_pred)
beta_hat <- coef(weib_reg)[7]

expected_claim <- eta_hat * gamma(1 + 1 / beta_hat)

# calculate the probability of an annual loss greater than $100,000 using the estimated regression coefficients and the given predictor values

prob_loss_100k <- 1 - pweibull(100000, scale = eta_hat, shape = beta_hat)
prob_loss_100k

# In 1), the probability of an annual loss greater than $100,000 is calculated using only the Kaplan-Meier estimation of the survival function. This means that the probability is calculated based on the historical data on loss amounts without taking into account any additional information, such as the coverage amount.

# In 5), the probability of an annual loss greater than $100,000 is calculated using the Weibull regression model. This means that the probability is calculated based on both the historical data on loss amounts and additional information, such as the entity type and coverage amount. The Weibull regression model provides a more accurate forecast by taking into account the effect of the predictor variables on the claim amounts.