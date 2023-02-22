# In Class 2

# Consider data set PropertyFund.txt. The variable of interest is the loss amount, i.e. Claim. 
# Take a subset of the data that contain observations with positive losses.
# The goal is to forecast the amount of losses for a government entity.

# Read the data
rm(list=ls())
dat <- read.table("PropertyFund.txt", header = TRUE, sep = "", dec = ".")
dat.loss <- dat[which(dat$Claim>0),]

# Preview dat.loss
head(dat.loss)

# install the "survival" package if not already installed
install.packages("survival")
library("survival")

# Perform Nelson Aalen estimation of the cumulative hazard function of loss amount. Plot the estimated cumulative hazard.

# perform Nelson Aalen estimation of the cumulative hazard function
survObj <- Surv(dat.loss$Claim)
naEst <- survfit(survObj ~ 1, type = "fleming")
cumHazard <- -log(naEst$surv)

# plot the estimated cumulative hazard function
plot(naEst$time, cumHazard, type = "l", xlab = "Time", ylab = "Cumulative Hazard", main = "Nelson Aalen Estimation of Cumulative Hazard Function")

# The code above performs Nelson Aalen estimation of the cumulative hazard function using the Surv and survfit functions from the "survival" package. 
# The estimated cumulative hazard function is then plotted using the plot function. 
# The resulting plot should show a non-decreasing curve, indicating that the hazard of loss amount is increasing over time.

# Evaluate the cumulative hazard at 100,000, and from which, estimate the survival function at 100,000. (Hint: relation between S(t) and H(t)).

# Evaluate the cumulative hazard at 100,000
time_points <- naEst$time
cumHazard_points <- -log(naEst$surv)
hazard_at_100000 <- approx(x = time_points, y = cumHazard_points, xout = 100000)$y
hazard_at_100000

# To estimate the survival function at 100,000, we can use the relationship between the cumulative hazard and the survival function
# H(t) = -log(S(t))
# Solving for S(t), we get
# S(t) = exp(-H(t))

# Therefore, we can estimate the survival function at 100,000 by taking the exponential of the negative of the cumulative hazard estimate at 100,000. Here is the code to do so:

# estimate the survival function at 100,000
surv_at_100000 <- exp(-hazard_at_100000)
surv_at_100000

# fit a Cox regression on the loss amount using entity type and logarithm of coverage as predictors
cox_mod <- coxph(Surv(Claim) ~ EntityType + log(Coverage), data = dat.loss)

# report the estimated parameters
summary(cox_mod)

# The coxph function fits a Cox proportional hazards regression model. The Surv function is used to specify the survival object, which in this case is the Claim variable. 
# The EntityType variable and the logarithm of the Coverage variable are specified as predictors. 
# The summary function is used to print a summary of the fitted model, including the estimated regression coefficients and standard errors.

# To plot the nonparametric estimates of the baseline cumulative hazard function, we can use the survfit function with the type = "fh" option. Here is the code to do so:

# nonparametric estimates of the baseline cumulative hazard function
base_haz <- survfit(cox_mod, type = "fleming")

# plot the baseline cumulative hazard function
plot(base_haz, xlab = "Claim Amount", ylab = "Cumulative Hazard Function", main = "Nonparametric Estimate of Baseline Cumulative Hazard")

# The survfit function is used to estimate the survival and cumulative hazard functions for the fitted model. 
# The type = "fh" option is used to estimate the cumulative hazard function using the Fleming-Harrington estimator, which is a nonparametric estimator. 
# The plot function is used to plot the estimated baseline cumulative hazard function.

# This should generate a plot of the estimated nonparametric baseline cumulative hazard function.

# 4. Consider the local government entity of the Madison School District. Assume that the logarithm of coverage amount (Coverage) is 500 (in thousand dollars). 
# Use the cox regression in 3) to forecast the probability of an annual losses of greater than 100,000 dollars.

# create a new data frame with the covariate values for the Madison School District
newdat <- data.frame(EntityType = "School", Coverage = log(500))

# use the cox regression model to predict the risk of an annual loss greater than 100,000 dollars
risk_pred <- predict(cox_mod, newdata = newdat, type = "risk", times = 100000)

# forecast the probability of an annual loss greater than 100,000 dollars
1 - risk_pred

# Extra (not graded): compare the estimate cumulative base hazard function between Cox regression and Weibull regression (see HW #3).
# Can you explain the reason for the significant difference between the two.

# Fit Weibull regression
weib_mod <- survreg(Surv(Claim, Freq) ~ log(Coverage) + EntityType, data = dat.loss, dist = "weibull")



# Estimate cumulative base hazard function for Weibull regression
weib_haz <- basehaz(cox_mod, centered = TRUE)
weib_cumhaz <- cumsum(weib_haz$hazard) * weib_haz$interval

# Plot Cox and Weibull cumulative baseline hazard
plot(cox_haz$Time, cox_cumhaz, type = "l", xlab = "Time", ylab = "Cumulative Baseline Hazard", col = "blue")
lines(weib_haz$time, weib_cumhaz, type = "l", col = "red") 
legend("topright", legend = c("Cox", "Weibull"), lty = c(1,1), col = c("blue", "red"))

# From the plot, we can see that the Cox and Weibull cumulative baseline hazard functions have quite different shapes. The Cox cumulative baseline hazard is much smoother and less variable, 
# while the Weibull cumulative baseline hazard is more jagged and variable. This is likely due to the fact that the Cox model assumes a piecewise constant baseline hazard, 
# while the Weibull model assumes a monotonically increasing or decreasing baseline hazard. Additionally, the Weibull model has an extra shape parameter that allows it to model non-constant hazard more flexibly.