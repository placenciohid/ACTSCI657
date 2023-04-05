rm(list = ls())
set.seed(123)
T <- 1000

# Create Vectors
z <- rep(NA, T+1)
sigma <- rep(NA, T+1)
y <- rep(NA, T+1)
y_stat <- rep(NA, T+1)
y_opt <- rep(NA, T+1)
y_pess <- rep(NA, T+1)

# Set value at t=1
sigma[1] = z[1] = 0

# Generate data and predictions
for (t in 2:(T+1)) {
  sigma[t] <- 0.2*z[t-1]^2 + 0.75*sigma[t-1] + 0.05
  z[t] <- rnorm(1, 0, sqrt(sigma[t]))
  y[t] <- z[t]^2
  y_stat[t] <- sigma[t]
  y_opt <- 5
  y_pess <- 0.5
}

# Calculate the squared-error, absolute-error, absolute percentage error, and relative error between y and y_stat, y_opt, y_pess for the loop

# Ignore the NA values
y <- y[!is.na(y)]
y_stat <- y_stat[!is.na(y_stat)]
y_opt <- y_opt[!is.na(y_opt)]
y_pess <- y_pess[!is.na(y_pess)]

# Calculate the squared-error, absolute-error, absolute percentage error, and relative error between y and y_stat, y_opt, y_pess for the loop
squared_error_stat <- sum((y - y_stat)^2)
squared_error_opt <- sum((y - y_opt)^2)
squared_error_pess <- sum((y - y_pess)^2)

absolute_error_stat <- sum(abs(y - y_stat))
absolute_error_opt <- sum(abs(y - y_opt))
absolute_error_pess <- sum(abs(y - y_pess))

absolute_percentage_error_stat <- sum(abs((y - y_stat)/y))
absolute_percentage_error_opt <- sum(abs((y - y_opt)/y))
absolute_percentage_error_pess <- sum(abs((y - y_pess)/y))

relative_error_stat <- sum(abs((y - y_stat)/y_stat))
relative_error_opt <- sum(abs((y - y_opt)/y_opt))
relative_error_pess <- sum(abs((y - y_pess)/y_pess))

# Create a data frame with the results
results <- data.frame(squared_error_stat, squared_error_opt, squared_error_pess, absolute_error_stat, absolute_error_opt, absolute_error_pess, absolute_percentage_error_stat, absolute_percentage_error_opt, absolute_percentage_error_pess, relative_error_stat, relative_error_opt, relative_error_pess)
