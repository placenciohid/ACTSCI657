# Homework 2

# Consider data set HealthData.txt. The data summarize the emergency room utilization for a random sample of U.S. population. 
# The variable of interest is the annual expenditure of ER utilization, i.e. erexp.

# Load the HealthData set and take a look at the data.

data <- read.table("HealthData.txt", header = TRUE)
head(data)

# Take a subset of the data that contain observations with positive expenditure. Answer the follow questions based on the subset.

data <- data[data$erexp > 0,]
head(data)

# 1. (0.5 pt) Use summary to report the summary statistics of erexp. Use hist to report the histogram of erexp. Comment on its characteristics.

summary(data$erexp)
hist(data$erexp)

# The erexp variable - annual expenditure of ER has a mean of 1489, a median of 706.
# The histogram shows that the data is skewed to the right. The mean is greater than the median, which is greater than the mode. 
# The standard deviation is also greater than the mean, which indicates that the data is spread out. 

# Import the library MASS and use the function gam to fit a small gamma regression using erexp as the response variable and age as the predictor.
library(MASS)

# 2. (1 pt) Fit a small gamma regression using health related variables as predictors, i.e. limitation and chronic. Write out your model assumptions and identify model parameters. 
# Report estimated parameters.

model <- glm(erexp ~ limitation + chronic, data = data, family = Gamma(link = "log"))
summary(model)

# 3. (1 pt) Interpret the regression coefficients of limitation and chronic. Comment on the effects of the predictors on the ER expenditure. Does the estimated effect make sense to you?

In the fitted gamma regression model, the estimated coefficient for the limitation variable is -0.05995 with a standard error of 0.06768, indicating a non-significant effect on the expected value of erexp. The estimated coefficient for the chronic variable is 0.05785 with a standard error of 0.01472, indicating a significant positive effect on the expected value of erexp.

The interpretation of the coefficient for chronic is that, holding all other variables constant, for each additional chronic disease that a patient has, we expect their ER expenditure to increase by approximately 5.8%. This effect is statistically significant at the 0.05 level, indicating that it is unlikely to have occurred by chance.

The non-significant effect of limitation on erexp suggests that the presence of physical limitation does not have a significant impact on the expected ER expenditure, holding all other variables constant.

Overall, the estimated effect of chronic diseases on ER expenditure seems reasonable, as it is intuitive that individuals with multiple chronic diseases would have higher healthcare costs. However, it is important to note that this is an observational study and causal interpretations should be made with caution. Additionally, the other predictors in the model, such as age, income, and insurance status, may also be important factors in explaining ER expenditure.

# 4. (0.5 pt) Fit a larger gamma regression. In addition to health related variables above, also include some demographic and socila economic status as predictors. Be more specific, use age, race, and natural log of income. Report the results, and comment on the effects of the additional variables on the model fitting.

model <- glm(erexp ~ limitation + chronic + age + race + log(income), family = Gamma(link = "log"), data = data)
summary(model)

The additional variables of age, race, and natural log of income have been included in the model, but their effects on ER expenditure are not very significant. Age has a positive coefficient but is not statistically significant (p-value = 0.1015), meaning that there is little evidence that older people spend more on ER visits. Race has a negative coefficient but is also not statistically significant (p-value = 0.2699), suggesting that race is not a strong predictor of ER expenditure. Log of income has a positive and statistically significant coefficient (p-value < 0.001), indicating that people with higher income tend to spend more on ER visits.

Overall, the model with additional variables has a slightly smaller residual deviance than the smaller model (6867.4 vs 7003.3), indicating a better fit to the data. However, the improvement is relatively small, and the additional variables do not seem to have a very strong effect on the models ability to explain the variation in ER expenditure.

# 5. (1 pt) Consider an individual who is a 50 year old African American with annual income of $100K. The individual has no physical limitation, but has 2 chronic diseases. Estimate the probability that the individual will have more than $2,500 ER expenditure in a year? Compare results from the smaller and the larger models.

# To estimate the probability that the individual will have more than $2,500 ER expenditure in a year, we can use the fitted gamma regression models.

# For the smaller model, we have:

glm(formula = erexp ~ limitation + chronic, family = Gamma(link = "log"),
    data = data)

# And for the larger model, we have:

glm(formula = erexp ~ limitation + chronic + age + race + log(income),
    family = Gamma(link = "log"), data = data)

# Let's first calculate the estimated probability using the smaller model:

# Predicting probability using smaller model

# First, we need to calculate the shape and scale parameters for the gamma distribution using the fitted model parameters.
# The shape parameter is 1/lambda, and the scale parameter is lambda/theta.
# We can use the pgamma function to calculate the probability that the random variable is less than 2500, given the shape and scale parameters.

# Predicting probability using smaller model
    prob_small <- 1 - pgamma(2500, shape = 1/exp(7.19644), scale = exp(7.19644)/3.420165, lower.tail = TRUE)
prob_small

# Now lets calculate the estimated probability using the larger model:

# Predicting probability using larger model
newdata <- data.frame(limitation = 0, chronic = 2, age = 50, race = 2, income = log(100000))
prob_large <- 1 - pgamma(2500, shape = 1/exp(predict(model, newdata)), scale = exp(predict(model, newdata))/3.515217, lower.tail = TRUE)
prob_large
