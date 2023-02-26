rm(list=ls())

library(survival)

library(SurvRegCensCov)


# Set working directory

setwd("~/Spring 2022 Courses/ACT SCI 657/HW3")
dat <- read.table(file='PropertyFund.txt', header = TRUE, sep = "", dec = ".")
dat.loss <- dat[which(dat$Claim>0),]

summary(dat.loss$Claim)

hist(dat.loss$Claim, xlab="Claims", main="")

fit.cdf <- ecdf(dat.loss$Claim)
plot(fit.cdf,main="")

# Kaplan-Meier

fit.sf <- survfit(Surv(Claim)~1, data=dat.loss, type = "kaplan-meier")
fit.sf

plot(summary(fit.sf)$time,summary(fit.sf)$surv,type="l",xlab="t",ylab="S(t)")

summary(fit.sf, times = 100000)

# Weibull

fit.wb <- WeibullReg(Surv(Claim)~ factor(EntityType) + log(Coverage), data=dat.loss)
fit.wb

# 4. Consider the local government entity of the Madison School District in 2). 
# Express the hazard function and the cumulative hazard function for the policyholder.

fit.wb$coef
coefwb <- fit.wb$coef

coefwb

fit.wb$summary

lam <- fit.wb$coef["lambda","Estimate"]

p <- fit.wb$coef["gamma" ,"Estimate"]

a <- 1/p
coefwb["factor(EntityType)School"]
reg <- coefwb["factor(EntityType)School", "Estimate"] * 1 + coefwb["log(Coverage)", "Estimate"] * log(500)

t <- 100000
St <- (exp(-lam*t^p))^(exp(reg))
St

# F(t)

Ft <- 1-St
Ft

# h(t)

lam*p*t^(p-1)

# H(t)

Ht <- -log(St)
Ht

# Integrate lam*p*t^(p-1) 
# Or just -ln(S(t))

lam*(t^p)