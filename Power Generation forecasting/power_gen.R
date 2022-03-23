
###########
#Power gen#
###########
#insample
train_data <- read.csv("data/Second bi-weekly assignment Train data.csv", TRUE, ";")
train_data$AirPassengers <- NULL
train_data_ts <- ts(APtrain_data, frequency = 24)
#wind <- read_csv('~/data/power wind.csv', TRUE)
#wind$day<-as.Date(wind$day,"%d/%m/%Y")
#power_ts <- ts(wind$power, freq = 24)
#speed_ts <- ts(wind$speed, freq = 24)

#outsample
test_data <- read.csv("data/Second bi-weekly assignment Test data.csv", TRUE, ";")
test_data$AirPassengers <- NULL
test_data_ts <- ts(test_data, frequency = 24)
#power_train <- ts(wind$power[1:2544],24)
#power_test <- ts(wind$power[2545:2568], 24)
#speed_train <- ts(wind$speed[1:2544], 24)
#speed_test <- ts(wind$speed[2545:2568], 24)
#train <- power_train+speed_train

png('Power Generation forecasting/Windspeed over time.png', res=300, width=15, height = 12, units = 'cm')
plot(train_data$speed, type = "l", col='red')
dev.off()

png('Power Generation forecasting/Power Generation over time.png', res=300, width=15, height = 12, units = 'cm')
plot(train_data$power, type = "l", col='green')
dev.off()

library(ggplot2)
library(ggpubr)

theme_set(
  theme_minimal() +
    theme(legend.position = "top")
)

b <- ggplot(train_data, aes(x = speed, y = power))
# Scatter plot with regression line
png('Power Generation forecasting/Wind Speed vs Power Gen.png', res=300, width=15, height = 12, units = 'cm')
b + geom_point()
dev.off()

cor(train_data$speed, train_data$power, method = c("pearson", "kendall", "spearman"))

#linear regression model
ml_model <- lm(Prices.BE~Month,data=train_data)
frc5_1 <- predict(ml_model,outsample_ml)
mean(200*abs(outsample_ml$Prices.BEfrc5_1)/(abs(outsample_ml$Prices.BE)+abs(frc5_1))) 

#
fit_lr <- lm(power ~ speed, data = train_data)
lrm <- as.numeric(predict(fit_1))

#
lr_model <- lm(power ~ speed, data=train_data)
frc_lr <- predict(lr_model,test_data)
mean(200*abs(test_data$power)/(abs(test_data$power)+abs(frc_lr))) 
sqrt(mean((test_data$power-frc_lr)^2))

png('Power Generation forecasting/Linear model vs test data.png', res=300, width=15, height = 12, units = 'cm')
plot(frc_lr, col='red')
plot(test_data$power, col='blue')
points(frc_lr, col='red') 
legend("topleft", legend=c("Test data","Linear model"), col = c("blue", "red"))
dev.off()


#Simple NN - neuralnet
# trainset_s$power <- (trainset_s$power-min(trainset_s$power))/(max(trainset_s$power)-min(trainset_s$power))
# trainset_s$speed <- (trainset_s$speed-min(trainset_s$speed))/(max(trainset_s$speed)-min(trainset_s$speed))
# testset_s$power <- (testset_s$power-min(trainset_s$power))/(max(trainset_s$power)-min(trainset_s$power))
# testset_s$speed <- (testset_s$speed-min(trainset_s$speed))/(max(trainset_s$speed)-min(trainset_s$speed))

trainset_s <- scale(train_data) #data used for training - scaled
testset_s <- scale(test_data) # data used for testing

library(neuralnet)
set.seed(7)
nn1 <- neuralnet(power ~ speed, data=trainset_s, 
                  hidden = 1, act.fct = "logistic", linear.output = FALSE)


frc_nn1_s <- as.numeric(predict(nn1, testset_s))
frc_nn1 <- frc_nn1_s*(max(train_data$power)-min(train_data$power))+min(train_data$power)
data.frame(frc_nn1,test_data$power)
sqrt(mean((test_data$power-frc_nn1)^2))

png('Power Generation forecasting/NN model vs test data.png', res=300, width=15, height = 12, units = 'cm')
plot(test_data$power, col='blue')
points(frc_nn1, col='red') 
legend("topleft", legend=c("Test data","NN"), col = c("blue", "red"))
dev.off()


#Regression tree
library(rpart)
rt <- rpart(power ~ speed, 
            method="anova", data=train_data)
frc_rt <- as.numeric(predict(rt, test_data))

sqrt(mean((test_data$power-frc_rt)^2))

png('Power Generation forecasting/Regression Tree vs test data.png', res=300, width=15, height = 12, units = 'cm')
plot(test_data$power, col='blue')
points(frc_rt, col='red') 
legend("topleft", legend=c("Test data","Regression Tree"), col = c("blue", "red"))
dev.off()

#Random forest
library(randomForest)
rf <- randomForest(power ~ speed, 
                   data = train_data, ntree = 1000)
frc_rf <- as.numeric(predict(rf, test_data))
sqrt(mean((test_data$power-frc_rf)^2))

png('Power Generation forecasting/Random Forest vs test data.png', res=300, width=15, height = 12, units = 'cm')
plot(test_data$power, col='blue')
points(frc_rf, col='red') 
legend("topleft", legend=c("Test data","Regression Tree"), col = c("blue", "red"))
dev.off()
