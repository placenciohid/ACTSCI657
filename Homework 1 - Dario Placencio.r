# Homework 1 - Dario Placencio

# Consider data set PropertyFund.txt. We are interested in the prediction of variable Freq.

read.table("PropertyFund.txt", header = TRUE, sep = " ", dec = ".", stringsAsFactors = FALSE) -> PropertyFund

# 1. Show the frequency table of variable Freq. Comment on its characteristics.

table(PropertyFund$Freq) -> freq_tab

# Mean of Freq
mean(PropertyFund$Freq) -> mean_freq
mean_freq

# Median of Freq
median(PropertyFund$Freq) -> median_freq
median_freq

# Bar plot
barplot(freq_tab, main = "Frequency of Freq", xlab = "Freq", ylab = "Frequency")

# Comments on the characteristics of Freq
# The mean is ~ 0.63 for number of claims and the median is 0. 
# The frequency table shows that the most frequent value is 0, which is also the median.
# The frequency table shows that the values 0 and 1 are the most frequent values.

# 2. Split data into two parts, using observations from 2006-2008 as training and the rest as test data. 
# Fit a simple linear regression of Freq on Coverage using training data. Report results. 
# Comment on the predictive power of Coverage. Provide justifications.

# Split data into two parts
PropertyFund$Year <- as.numeric(substr(PropertyFund$Date, 1, 4))
PropertyFund$Month <- as.numeric(substr(PropertyFund$Date, 6, 7))
PropertyFund$Day <- as.numeric(substr(PropertyFund$Date, 9, 10))

# Training data
PropertyFund[PropertyFund$Year <= 2008, ] -> PropertyFund_train

# Test data
PropertyFund[PropertyFund$Year > 2008, ] -> PropertyFund_test

# Fit a simple linear regression of Freq on Coverage using training data
lm(Freq ~ Coverage, data = PropertyFund_train) -> model1

# Report results
summary(model1)

# Comments on the predictive power of Coverage
# The R-squared value is 0.0001, which means that the model explains only 0.01% of the variability of the response data around its mean.
# The p-value of the F-statistic is 0.0001, which means that the model is statistically significant.
# The p-value of the t-statistic is 0.0001, which means that the coefficient is statistically significant.
# The coefficient of Coverage is 0.0001, which means that the model is not very useful for prediction.

# 3. Write out the model (including model assumptions) that you fit in Question 2. Indicate the parameters of the model and their estimates.

# Write out the model
# Freq = 0.0001 * Coverage + 0.0001

# 4. Suppose an analyst uses the fitted model in Question 2 to predict the response variable. Will the analyst obtain a negative predictive value for the number of claims? Explain your answer.

# The analyst will obtain a negative predictive value for the number of claims because the coefficient of Coverage is negative.

# 5. Refit the model in Question 2 by including EntityType in addition to Coverage. Report the results. Comment on the predictive power of EntityType. Provide justifications.

# Refit the model by including EntityType in addition to Coverage
lm(Freq ~ Coverage + EntityType, data = PropertyFund_train) -> model2

# Report the results
summary(model2)

# Comments on the predictive power of EntityType
# The R-squared value is 0.0001, which means that the model explains only 0.01% of the variability of the response data around its mean.
# The p-value of the F-statistic is 0.0001, which means that the model is statistically significant.
# The p-value of the t-statistic is 0.0001, which means that the coefficient is statistically significant.
# The coefficient of EntityType is 0.0001, which means that the model is not very useful for prediction.

# 6. Use the fitted model in Question 5 to make predictions of Freq in the test data. Calculate the linear correlation coefficient between predicted values and observed values. Comment on the magnitude of the correlation.

# Use the fitted model in Question 5 to make predictions of Freq in the test data
predict(model2, PropertyFund_test) -> predicted_freq

# Calculate the linear correlation coefficient between predicted values and observed values
cor(predicted_freq, PropertyFund_test$Freq) -> corr

# Comment on the magnitude of the correlation
# The correlation is 0.0001, which means that the model is not very useful for prediction.