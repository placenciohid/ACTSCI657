# Set work encvironment
#setwd("C:/Users/Dario/Documents/Github/ACTSCI657")

# Read data txt
data <- read.table("HealthData.txt", header = TRUE)

# Take a subset of the data that contain observations with positive expenditure. 
data <- data[data$erexp > 0, ]

# Further split data into two parts, using observations in 2010 as training and observations in 2011 as test data.
train <- data[data$year == 2010, ]
test <- data[data$year == 2011, ]

# Import the package for gamma regression
library(MASS)

# Fit a gamma regression over the erexp variable. Use predictors chronic, race, and log of income in the model.
glm(erexp ~ chronic + race + log(income), data = train, family = Gamma(link = "log")) -> model1

# Report results
summary(model1)

# Identify a potential offset and/or weight variable. Refit the model by incorporating the offset/weight.
glm(erexp ~ chronic + race + log(income), data = train, family = Gamma(link = "log"), weights = sqrt(ernum), offset = log(ernum)) -> model2
summary(model2)

# Use mean as the predicted value. Predict the ER expenditure for individuals in the test data. 
# Compare the prediction between above two models using mean squared prediction error (MSPE), i.e. ∑i=(yi−y^i)2. 
# Indicated the preferred model suggested by MSPE

# Use mean as the predicted value
mean(test$erexp) -> mean_erexp

# Predict the ER expenditure for individuals in the test data
predict(model1, test, type='response') -> predicted_erexp1
predicted_erexp1

# Calculate the mean squared prediction error
mean((mean_erexp - predicted_erexp1)^2) -> mspe1
mspe1

# Use the fitted model to predict the ER expenditure for individuals in the test data
predict(model2, test, type='response') -> predicted_erexp2
predicted_erexp2

# Calculate the mean squared prediction error
mean((mean_erexp - predicted_erexp2)^2) -> mspe2
mspe2

# Use the 80th percentile as the predicted value. Predict the ER expenditure for individuals in the test data. 
# Compare the prediction between above two models using mean squared prediction error (MSPE), i.e. ∑i=(yi−y^i)2. 
# Indicated the preferred model suggested by MSPE.

# Use the 80th percentile as the predicted value
quantile(train$erexp, 0.8) -> percentile_erexp

# Predict the ER expenditure for individuals in the test data
predict(model1, test, type='response') -> predicted_erexp3
predicted_erexp3

# Calculate the mean squared prediction error
mean((percentile_erexp - predicted_erexp3)^2) -> mspe3
mspe3   

# Use the fitted model to predict the ER expenditure for individuals in the test data
predict(model2, test, type='response') -> predicted_erexp4
predicted_erexp4

# Calculate the mean squared prediction error
mean((percentile_erexp - predicted_erexp4)^2) -> mspe4
mspe4

# Indicate the preferred model suggested by MSPE
if (mspe1 < mspe2) {
  print("The preferred model is model1")
} else {
  print("The preferred model is model2")
}

# Indicate the preferred model suggested by MSPE    
if (mspe3 < mspe4) {
  print("The preferred model is model1")
} else {
  print("The preferred model is model2")
}