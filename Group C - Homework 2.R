# Set the folder (path) that contains this R file as the working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path);
setwd(dir);

library(fBasics);
library(forecast);

## Adjust plotting area
par(mfrow = c(3, 1));

#### SERIES 1 ####
data <- read.csv("coca_cola_earnings.csv", header=TRUE, sep = ";", dec = ",");

x <- data[,2];
length(x);

x <- data[1:83,2];

val <- data[84:107,2];

## 1) Visually check the data for mean and variance stationarity

ts.plot(x); ## The data is not stationary in variance, neither mean

## 2) Transform data to logarithmic scale to make variance stationary

y <- log(x);

ts.plot(y);

## 3) Tests to check how many differences do we need to take

ndiffs(y, alpha = 0.05, test = "adf"); ## First difference = 1

nsdiffs(y, m = 4, test = c("ocsb")); ## Seasonal difference = 1

z <- diff(y);

## 4) Visually check new data for stationarity

ts.plot(z); ## Stationary in both mean and variance

## 5) Check lags out of bounds on ACF and PACF

acf(z, lag.max = 20); ## Clearly seasonal - 2, 4, 6, 8, etc. out of bounds
pacf(z, lag.max = 20); ## 2, 3, 4, 5, 8, 9 out of bounds

## 6) Fit model and check residuals

#### MODEL 1 - a, b, c ####

fit_a <- arima(y, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 4)); ## Option 1

fit_a; ## MA1 and SMA1 are significant - 0 is out of range

ts.plot(fit_a$residuals);

## 7) Check if residuals have lags out of bounds

acf(fit_a$residuals, lag.max = 20); ## 1 lag out of bounds #5
pacf(fit_a$residuals, lag.max = 20); ## 1 lag out of bounds #5

Box.test(fit_a$residuals, lag = 5); ## P-value on lag 5 is higher than 0.05, we can consider the lag out of bounds as WN and proceed with this model

## 8) Shapiro test to check if the residuals are normally distributed, confirming White Noise

Box.test(fit_a$residuals); ## P-value is higher than 0.05, meaning residuals are WN

shapiro.test(fit_a$residuals) ## P-value is not higher than 0.05, meaning the residuals are not GWN - we need to check real Z values for interval predictions

## 9) Predictions

a.pred <- predict(fit_a, n.ahead = 24);
a.pred_pred <- exp(a.pred$pred); ## Transforming data back from logarithmic scale

b.pred <- exp(a.pred$pred)
ts.plot(c(x, b.pred));
lines(b.pred, col = "green");

#### MODEL 2 - j, k, l ####

fit_j <- arima(y, order = c(2, 1, 2), seasonal = list(order = c(0, 1, 2), period = 4)); ## Option 2

fit_j; ## AR2, MA2 and SMA2 are significant - 0 is out of range

ts.plot(fit_j$residuals);

## 7) Check if residuals have lags out of bounds

acf(fit_j$residuals, lag.max = 20); ## No lags out of bounds
pacf(fit_j$residuals, lag.max = 20); ## No lags out of bounds

## 8) Shapiro test to check if the residuals are normally distributed, confirming White Noise

Box.test(fit_j$residuals); ## P-value is higher than 0.05, meaning residuals are WN

shapiro.test(fit_j$residuals) ## P-value is not higher than 0.05, meaning the residuals are not GWN - we need to check real Z values for interval predictions

## 9) Predictions

j.pred <- predict(fit_j, n.ahead = 24);
j.pred_pred <- exp(j.pred$pred); ## Transforming data back from logarithmic scale

k.pred <- exp(j.pred$pred)
ts.plot(c(x, k.pred));
lines(k.pred, col = "green");

## 10) Model evaluation & selection

mape_option1 <- round(mean(abs((a.pred_pred - val)/val))*100, 4);
mape_option2 <- round(mean(abs((j.pred_pred - val)/val))*100, 4);

mape_option1;
mape_option2;

msfe_option1 <- round(mean((a.pred_pred - val)^2), 4);
msfe_option2 <- round(mean((j.pred_pred - val)^2), 4);

msfe_option1;
msfe_option2;

models <- c("Model", "(0,1,1)(0,1,1)[4]", "(2,1,2)(0,1,2)[4]")

model_selection <- data.frame();
header <- c("Model", "MAPE", "MSFE");

model_selection <- as.data.frame(models);
model_selection <- cbind(model_selection, c("MAPE", mape_option1, mape_option2));
model_selection <- cbind(model_selection, c("MSFE", msfe_option1, msfe_option2));

colnames(model_selection) <- header;

model_selection <- model_selection[2:3,];

View(model_selection)

## Based on these results, we would pick Model 1 - SARIMA (0,1,1)(0,1,1)[4]
