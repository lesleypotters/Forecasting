library(readr)
train_data <- read.csv("data/Second bi-weekly assignment Train data.csv", TRUE, ";")
test_data <- read.csv("data/Second bi-weekly assignment Test data.csv", TRUE, ";")

AirPassengers_train <- ts(train_data$AirPassengers, start = c(1949,1), end = c(1959,12), frequency=12)
AirPassengers_test <- ts(test_data$AirPassengers, start = c(1960,1), end = c(1960,12), frequency=12)

AirPassengers <- ts(c(AirPassengers_train, AirPassengers_test),start = start(AirPassengers_train),frequency = frequency(AirPassengers_train))


##Visualize the original series (of the store sales)
png('Air Passengers forecasting/plot air passengers', res=300, height = 12, width = 15, units = 'cm')
plot(AirPassengers_train, main = "Air Passengers", xlab="Date", ylab="Passengers")
dev.off()
##Decompose
dec_m <- decompose(AirPassengers_train, type="multiplicative")$seasonal
png('Air Passengers forecasting/decompose mult seasonal', res=300, height = 12, width = 15, units = 'cm')
plot(dec_m)
dev.off()

dec_a <- decompose(AirPassengers_train, type="additive")$seasonal
png('Air Passengers forecasting/decompose add seasonal', res=300, height = 12, width = 15, units = 'cm')
plot(dec_a)
dev.off()


# Check for heteroskedacity
dec <- decompose(AirPassengers_train, type="additive") 
d_time_series <- AirPassengers_train - dec$seasonal

png('Air Passengers forecasting/deseasonalized', res=300, height = 12, width = 15, units = 'cm')
plot(AirPassengers_train, type="l", main="Passenger",
     ylab = "Number of Passengers", xlab = "Year")
lines(d_time_series, col="red")
legend("topleft",legend = c("Series", "Deseasonalized Series"),col = c("black", "red"), lty=1)
dev.off()

#####
#SES#
#####

#SES with different alpha values
ses_check <- ses(AirPassengers_train)
ses1 <- ses(AirPassengers_train, alpha = 0.2, h=12, initial = "optimal")
ses2 <- ses(AirPassengers_train, alpha = 0.8, h=12)
ses3 <- ses(AirPassengers_train, alpha = 0.95, h=12)

png('Air Passengers forecasting/ses alpha 0.20', res=300, height = 12, width = 15, units = 'cm')
plot(AirPassengers, type="l")
plot(ses1)
lines(ses1$fitted, col="red")
lines(ses1$mean, col="red")
dev.off()

png('Air Passengers forecasting/ses alpha 0.80', res=300, height = 12, width = 15, units = 'cm')
plot(ses2)
lines(ses2$fitted, col="blue")
lines(ses2$mean, col="blue")
dev.off()

png('Air Passengers forecasting/ses alpha 0.95', res=300, height = 12, width = 15, units = 'cm')
plot(ses3)
lines(ses3$fitted, col="green")
lines(ses3$mean, col="green")
dev.off()
#legend("topleft", legend=c("a=0.2", "a=0.8", "a=0.95"),
#       col=c("red", "blue", "green"), lty=1, cex=0.8)

#Insample accuracy

alpha <- seq(.01, .99, by = .01)
RMSE <- NA
for(i in seq_along(alpha)) {
  fit <- ses(AirPassengers_train, alpha = alpha[i], h = 12)
  RMSE[i] <- accuracy(fit, AirPassengers_test)[2,2]
}


# convert to a data frame and identify min alpha value
library(tibble)
library(fpp)      
alpha.fit <- data_frame(alpha, RMSE)
alpha.min <- filter(alpha.fit, RMSE == min(RMSE))

# plot RMSE vs. alpha
b <- ggplot(alpha.fit, aes(alpha, RMSE)) + geom_line() + geom_point(data = alpha.min, aes(alpha, RMSE), size = 2, color = "blue")
b + geom_point()

round(accuracy(ses1,2))
round(accuracy(ses2,2))
round(accuracy(ses3,2))

rmse_in1_ses1 <- sqrt(mean((AirPassengers_train - ses1$fitted)^2))
rmse_in2_ses2 <- sqrt(mean((AirPassengers_train - ses2$fitted)^2))
rmse_in3_ses3 <- sqrt(mean((AirPassengers_train - ses3$fitted)^2))

#Outsample accuracy
rmse_out1_ses1 <- sqrt(mean((AirPassengers_test - ses1$mean)^2))
rmse_out2_ses2 <- sqrt(mean((AirPassengers_test - ses2$mean)^2))
rmse_out3_ses3 <- sqrt(mean((AirPassengers_test - ses3$mean)^2))

######
#holt#
######

library(forecast)
holt1 <- holt(AirPassengers_train, h=12)

autoplot(holt1)
holt1$model

round(accuracy(holt1),2)

plot(AirPassengers, type="l")
lines(holt1$fitted, col="red")
lines(holt1$mean, col="red")


#damped exponential smoothing
holt2 <- holt(AirPassengers_train, damped = TRUE, h=12)
autoplot(holt2)
holt2$model

lines(holt2$fitted, col="blue")
lines(holt2$mean, col="blue")


round(accuracy(holt2),2)


#Insample accuracy
rmse_in1 <- sqrt(mean((AirPassengers_train - fit1$fitted)^2))
rmse_in2 <- sqrt(mean((AirPassengers_train - fit2$fitted)^2))
#Outsample accuracy
mse_out1 <- mean((AirPassengers_test - fit1$mean)^2)
mse_out2 <- mean((AirPassengers_test - fit2$mean)^2)

