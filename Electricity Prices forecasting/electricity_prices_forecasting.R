library(readr)
train_data <- read.csv("data/TrainSet.csv", TRUE, ",")
test_data <- read.csv("data/TestSet.csv", TRUE, ",")

#plot
png('Electricity Prices forecasting/Electricity Prices.png', res=300, width=15, height = 12, units = 'cm')
plot(train_data$Prices.BE, main = "Electricity Prices", xlab="Date", ylab="Prices in €")
dev.off()


##To identify the missing values and fill them using an appropriate method.
library(ggplot2)
library(imputeTS)

png('Electricity Prices forecasting/Missing Values.png', res=300, width=15, height = 12, units = 'cm')
ggplot_na_distribution(train_data$Prices.BE)
dev.off()

which(is.na(train_data$Prices.BE))
library(imputeTS)

train_data$Prices.BE <- na_interpolation(train_data$Prices.BE)

#plot with removed values
train_data <- train_data[which(train_data$Prices.BE < 2000),]
train_data <- train_data[which(train_data$Prices.BE > 0),]

png('Electricity Prices forecasting/Electricity Prices improved.png', res=300, width=15, height = 12, units = 'cm')
plot(train_data$Prices.BE, main = "Electricity Prices without outliers and missing values", xlab="Date", ylab="Prices in €")
dev.off()

#corr
library(corrplot)
M <- cor(train_data[sapply(train_data, is.numeric)])

png('Electricity Prices forecasting/correlation plot.png', res=300, width=15, height = 12, units = 'cm')
corrplot(M, method = 'color', order = 'alphabet') 
dev.off()

#scatter
png('Electricity Prices forecasting/scatter power gen vs electr prices BE.png', res=300, width=15, height = 12, units = 'cm')
plot(train_data$Generation_BE, train_data$Prices.BE, main="Scatterplot Power Gen BE vs Electricity Price BE",
     xlab="Power Generation ", ylab="Electricity Prices", pch=19)
dev.off()

png('Electricity Prices forecasting/scatter power gen vs electr prices FR.png', res=300, width=15, height = 12, units = 'cm')
plot(train_data$Generation_FR, train_data$Prices.BE, main="Scatterplot Power Gen FR vs Electricity Price FR",
     xlab="Power Generation ", ylab="Electricity Prices", pch=19)
dev.off()

png('Electricity Prices forecasting/scatter holidays elec prices.png', res=300, width=15, height = 12, units = 'cm')
plot(train_data$holidaysBE, train_data$Prices.BE, main="Scatterplot BE holidays vs Electricity Price BE",
     xlab="Power Generation ", ylab="Electricity Prices", pch=19)
dev.off()

#declare time series 
library(lubridate)
train_data$datetime <- as.POSIXct(strptime(train_data$datetime_utc, "%Y-%m-%d %H:%M:%S"))
test_data$datetime <- as.POSIXct(strptime(test_data$datetime_utc, "%Y-%m-%d %H:%M:%S"))

train_data$hour = hour(train_data$datetime)
train_data$year = year(train_data$datetime)
train_data$month = month(train_data$datetime)
train_data$weekday = wday(train_data$datetime)

test_data$hour = hour(test_data$datetime)
test_data$year = year(test_data$datetime)
test_data$month = month(test_data$datetime)
test_data$weekday = wday(test_data$datetime)

#plots per time unit
hourly = aggregate(train_data$Prices.BE,
                  by = list(train_data$hour),
                  FUN = mean)

dayofweek = aggregate(train_data$Prices.BE,
                  by = list(train_data$weekday),
                  FUN = mean)
weekly = aggregate(train_data$Prices.BE,
                  by = list(train_data$week),
                  FUN = mean)
monthly = aggregate(train_data$Prices.BE,
                  by = list(train_data$month),
                  FUN = mean)
yearly = aggregate(train_data$Prices.BE,
                  by = list(train_data$year),
                  FUN = mean)

png('Electricity Prices forecasting/hourly prices.png', res=300, width=15, height = 12, units = 'cm')
plot(hourly,main = "Average Electricity Prices by hour", xlab="Date", ylab="Energy Price")
dev.off()

png('Electricity Prices forecasting/day of week prices.png', res=300, width=15, height = 12, units = 'cm')
plot(dayofweek ,main = "Average Electricity Prices by day of the week", xlab="Date", ylab="Energy Price")
dev.off()

png('Electricity Prices forecasting/weekly prices.png', res=300, width=15, height = 12, units = 'cm')
plot(weekly,main = "Average Electricity Prices by week", xlab="Date", ylab="Energy Price")
dev.off()

png('Electricity Prices forecasting/monthly prices.png', res=300, width=15, height = 12, units = 'cm')
plot(monthly, main = "Electricity Prices by month", xlab="Date", ylab="Energy Price")
dev.off()

png('Electricity Prices forecasting/yearly prices.png', res=300, width=15, height = 12, units = 'cm')
plot(yearly, main = "Electricity Prices by year", xlab="Date", ylab="Energy Price")
dev.off()


#####################
#statistical methods#
#####################

#Prepare data
#Examine possible scenarios for producing forecasts
timeseries <- ts(train_data$Prices.BE, frequency=168)
fh <- 168
insample <- head(timeseries,length(timeseries)-fh) 
outsample <- tail(timeseries,fh)

Evaluation <- data.frame(matrix(NA, ncol = 1, nrow = 17))
row.names(Evaluation) <- c("Naive","SES","sNaive","SES_Mul","Theta", "MLR-M","MLR-MW","MLR-MWH","MLR-MWHHo","MLR-MWHGB","MLR-MWHGF","MLR-MWHGBF","MLR-MWHGL","NN 1 layer","NN 4 layers", "xgboost", "Comb" ) 
colnames(Evaluation) <- c("sMAPE")

#Naive
frc1 <- naive(insample,h=fh)$mean
Evaluation$sMAPE[1] <- mean(200*abs(outsample-frc1)/(abs(outsample)+abs(frc1)))
#SES - no decomposition
frc2 <- ses(insample,h=fh)$mean
Evaluation$sMAPE[2] <- mean(200*abs(outsample-frc2)/(abs(outsample)+abs(frc2)))
#Seasonal Naïve
frc3 <- as.numeric(tail(insample,fh)) + outsample - outsample
Evaluation$sMAPE[3] <- mean(200*abs(outsample-frc3)/(abs(outsample)+abs(frc3)))
#SES - with decomposition
Indexes_in <- decompose(insample, type = "multiplicative")$seasonal
Indexes_out <- as.numeric(tail(Indexes_in,fh))
frc4 <- ses(insample/Indexes_in,h=fh)$mean*Indexes_out
Evaluation$sMAPE[4] <- mean(200*abs(outsample-frc4)/(abs(outsample)+abs(frc4)))
#Theta
theta_prediction <- thetaf(insample,h=fh)$mean
Evaluation$sMAPE[5] <- mean(200*abs(outsample-theta_prediction)/(abs(outsample)+abs(theta_prediction)))

#Inspect results
png('Electricity Prices forecasting/statistical methods.png', res=300, width=15, height = 12, units = 'cm')
plot(outsample)
lines(frc1, col=2) ; lines(frc2, col=3)
lines(frc3, col=4) ; lines(frc4, col=5)
lines(theta_prediction, col=6) 
legend("topleft", legend=c("Naive","SES","sNaive","SES_Mul", 'Theta'),col=c(2:6), lty=1, cex=0.8) 
dev.off()
Evaluation


#####
#MLR#
#####


#Sales_data$`Company Sales` <- na_interpolation(Sales_data$`Company Sales`)
library(forecast)

#preparing data - hour, weekday, month, year, holiday, power gen FR&BE, lagged prices
Data_ml <- train_data

Data_ml$Lag168 = Data_ml$Lag336 <- NA #Define Level
Data_ml$Lag168 <- head(c(rep(NA,168), head(Data_ml,nrow(Data_ml)-168)$Prices.BE),nrow(Data_ml)) 
Data_ml$Lag336 <- head(c(rep(NA,336), head(Data_ml,nrow(Data_ml)-336)$Prices.BE),nrow(Data_ml)) 
Data_ml <- na.omit(Data_ml) #Delete NAs
insample_ml <- head(Data_ml,nrow(Data_ml)-fh) #in-sample for training
outsample_ml <- tail(Data_ml,fh) #out-of-sample for testing

#Inspect Correlations
library(corrplot)
png('Electricity Prices forecasting/corr for ml.png', res=300, width=15, height = 12, units = 'cm')
corrplot(cor(insample_ml[,-c(1,6,7,8)]), method="color")
dev.off()

#Only Month
ml_model <- lm(Prices.BE~month,data=insample_ml) 
frc5_1 <- predict(ml_model,outsample_ml) 
mean(200*abs(outsample_ml$Prices.BE- frc5_1)/(abs(outsample_ml$Prices.BE)+abs(frc5_1)))
Evaluation$sMAPE[6] <- mean(200*abs(outsample-frc5_1)/(abs(outsample)+abs(frc5_1)))

#Only Month and Weekday
ml_model <- lm(Prices.BE~month+weekday,data=insample_ml) 
frc5_2 <- predict(ml_model,outsample_ml) 
mean(200*abs(outsample_ml$Prices.BE- frc5_2)/(abs(outsample_ml$Prices.BE)+abs(frc5_2)))
Evaluation$sMAPE[7] <- mean(200*abs(outsample-frc5_2)/(abs(outsample)+abs(frc5_2)))

#Only Month and Weekday and hour
ml_model <- lm(Prices.BE~month+weekday+hour,data=insample_ml) 
frc5_3 <- predict(ml_model,outsample_ml) 
mean(200*abs(outsample_ml$Prices.BE- frc5_3)/(abs(outsample_ml$Prices.BE)+abs(frc5_3)))
Evaluation$sMAPE[8] <- mean(200*abs(outsample-frc5_3)/(abs(outsample)+abs(frc5_3)))

#Only Month and Weekday and hour and holidays
ml_model <- lm(Prices.BE~month+weekday+hour+holidaysBE,data=insample_ml) 
frc5_4 <- predict(ml_model,outsample_ml) 
mean(200*abs(outsample_ml$Prices.BE- frc5_4)/(abs(outsample_ml$Prices.BE)+abs(frc5_4)))
Evaluation$sMAPE[9] <- mean(200*abs(outsample-frc5_4)/(abs(outsample)+abs(frc5_4)))

#Only Month and Weekday and hour and holidays and generation
ml_model <- lm(Prices.BE~month+weekday+hour+holidaysBE+Generation_BE,data=insample_ml) 
frc5_5_1 <- predict(ml_model,outsample_ml) 
mean(200*abs(outsample_ml$Prices.BE- frc5_5_1)/(abs(outsample_ml$Prices.BE)+abs(frc5_5_1)))
Evaluation$sMAPE[10] <- mean(200*abs(outsample-frc5_5_1)/(abs(outsample)+abs(frc5_5_1)))

ml_model <- lm(Prices.BE~month+weekday+hour+holidaysBE+Generation_FR,data=insample_ml) 
frc5_5_2 <- predict(ml_model,outsample_ml) 
mean(200*abs(outsample_ml$Prices.BE- frc5_5_2)/(abs(outsample_ml$Prices.BE)+abs(frc5_5_2)))
Evaluation$sMAPE[11] <- mean(200*abs(outsample-frc5_5_2)/(abs(outsample)+abs(frc5_5_2)))

ml_model <- lm(Prices.BE~month+weekday+hour+holidaysBE+Generation_BE+Generation_FR,data=insample_ml) 
frc5_5_3 <- predict(ml_model,outsample_ml) 
mean(200*abs(outsample_ml$Prices.BE- frc5_5_3)/(abs(outsample_ml$Prices.BE)+abs(frc5_5_3)))
Evaluation$sMAPE[12] <- mean(200*abs(outsample-frc5_5_3)/(abs(outsample)+abs(frc5_5_3)))

#Only Month, Weekday, Hour, BE Generation and Lags
ml_model <- lm(Prices.BE~month+hour+weekday+ Generation_BE+Lag168+Lag336,data=insample_ml) 
frc5_6 <- predict(ml_model,outsample_ml) 
mean(200*abs(outsample_ml$Prices.BE- frc5_6)/(abs(outsample_ml$Prices.BE)+abs(frc5_6)))
Evaluation$sMAPE[13] <- mean(200*abs(outsample-frc5_6)/(abs(outsample)+abs(frc5_6)))
Evaluation

#Inspect MLR
plot(outsample)
lines(frc5_1+outsample-outsample, col=2) 
lines(frc5_2+outsample-outsample, col=3)
lines(frc5_3+outsample-outsample, col=4) 
lines(frc5_4+outsample-outsample, col=5) 
lines(frc5_5_1+outsample-outsample, col=6) 
lines(frc5_5_2+outsample-outsample, col=7) 
lines(frc5_5_3+outsample-outsample, col=8) 
lines(frc5_6+outsample-outsample, col=9)
legend("topleft", legend=c("MLR-M","MLR-MW","MLR-MWH", "MLR-MWHHo","MLR-MWHG","MLR-MWHGL"),
col=c(2:7), lty=1, cex=0.8)
Evaluation

legend("topleft", legend=c("MLR-M","MLR-MW","MLR-MWH", "MLR-MWHHo","MLR-MWHG","MLR-MWHGL"),col=c(2:7), lty=1, cex=0.8)

####
#NN#
####

#NN
ForScaling <- rbind(insample_ml,outsample_ml)[,c("Generation_BE","Lag168","Lag336", "month","hour","weekday")]
ForScaling$Generation_BE <- ((ForScaling$Generation_BE - min(insample_ml$Generation_BE)) / (max(insample_ml$Generation_BE) - min(insample_ml$Generation_BE))) 
ForScaling$Lag168 <- ((ForScaling$Lag168 - min(insample_ml$Lag168)) / (max(insample_ml$Lag168) - min(insample_ml$Lag168)))
ForScaling$Lag336 <- ((ForScaling$Lag336 - min(insample_ml$Lag336)) / (max(insample_ml$Lag336) - min(insample_ml$Lag336)))

#Create dummy variables
library(caret)
dummy <- dummyVars(" ~ .", data=ForScaling[,c("month","hour","weekday")])
dummy <- data.frame(predict(dummy, newdata = ForScaling[,c("month","hour","weekday")])) 
dummy$month.1= dummy$hour.0 = dummy$weekday.1 <- NULL
ForScaling$month = ForScaling$hour = ForScaling$weekday <- NULL
ForScaling <- cbind(ForScaling, dummy)

trainNN_x <- head(ForScaling, nrow(ForScaling)-fh)
trainNN_y <- ((insample_ml$Prices.BE - min(insample_ml$Prices.BE)) / (max(insample_ml$Prices.BE) - min(insample_ml$Prices.BE))) 
testNN_x <- tail(ForScaling, fh)
testNN_y <- outsample_ml$Prices.BE

library(RSNNS)
#Single layer
set.seed(101) 
model1<-mlp(trainNN_x, trainNN_y,size = 20, maxit = 100,initFunc = "Randomize_Weights",learnFunc = "BackpropWeightDecay", hiddenActFunc = "Act_Logistic", shufflePatterns = FALSE, linOut = FALSE)

frc6_1 <- as.numeric(predict(model1,testNN_x))*(max(insample_ml$Prices.BE) - min(insample_ml$Prices.BE)) + min(insample_ml$Prices.BE) 
mean(200*abs(testNN_y-frc6_1)/(abs(testNN_y)+abs(frc6_1)))
Evaluation$sMAPE[14] <- mean(200*abs(outsample-frc6_1)/(abs(outsample)+abs(frc6_1)))
lines(frc6_1+outsample-outsample, col=10)
Evaluation
#4 layers 20 15 10 5
set.seed(101) 
model2<-mlp(trainNN_x, trainNN_y,size = c(20,15,10,5), maxit = 100,initFunc = "Randomize_Weights", learnFunc = "BackpropWeightDecay", hiddenActFunc = "Act_Logistic", shufflePatterns = FALSE, linOut = FALSE)
frc6_2 <- as.numeric(predict(model2,testNN_x))*(max(insample_ml$Prices.BE) - min(insample_ml$Prices.BE)) + min(insample_ml$Prices.BE) 
mean(200*abs(testNN_y-frc6_2)/(abs(testNN_y)+abs(frc6_2)))
Evaluation$sMAPE[15] <- mean(200*abs(outsample-frc6_2)/(abs(outsample)+abs(frc6_2)))

plot(outsample[, 'Prices.BE'],type="l", main="NN")
lines(frc6_2, col=11)

#4 layers 64 32 16 8
set.seed(100)
NNmodel <-mlp(trainNN_x, trainNN_y,size = c(64,32,16,8), maxit = 100,initFunc ="Randomize_Weights",learnFunc = "BackpropWeightDecay", hiddenActFunc = "Act_Logistic",
              shufflePatterns = FALSE, linOut = FALSE) 
NNpred <- as.numeric(predict(NNmodel,testNN_x))*(max(insample_ml$Prices.BE) - min(insample_ml$Prices.BE)) + min(insample_ml$Prices.BE)
mean(200*abs(testNN_y-NNpred)/(abs(testNN_y)+abs(NNpred)))
lines(NNpred+outsample-outsample, col="red",type="l")

#xgboost
library(xgboost)
bstDense <- xgboost(data = as.matrix(trainNN_x), label = trainNN_y, max.depth = 20, eta = .3, nthread = 5, nrounds = 100, objective = "reg:linear")
pred <- predict(bstDense, as.matrix(testNN_x))*(max(insample_ml$Prices.BE) - min(insample_ml$Prices.BE)) + min(insample_ml$Prices.BE)
mean(200*abs(testNN_y-pred)/(abs(testNN_y)+abs(pred)))
lines(pred+outsample-outsample, col=13)
Evaluation$sMAPE[16] <- mean(200*abs(outsample-pred)/(abs(outsample)+abs(pred)))
Evaluation

