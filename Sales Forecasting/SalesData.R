#import the data
library(readr)
Sales_data<- read_csv("data/Bi-weekly assignment data.csv")

#Exercise A
##Visualize the original series (of the store sales).
data <- Sales_data$`Company Sales`
product_data <- Sales_data$`Product Sales`

series <- ts(as.numeric(data), frequency=365, start = c(2019,1))
prseries <- ts(as.numeric(product_data), frequency=365, start = c(2019,1))

png('Sales Forecasting/CompanySalesPerDay.png', res=300,width = 15, height = 12, units = 'cm')
plot(series, main = "Company Sales per day", xlab="Date", ylab="Company Sales")
dev.off()

png('Sales Forecasting/ProductSalesPerDay.png', res=300,width = 15, height = 12, units = 'cm')
plot(prseries, main = "Product Sales per day", xlab="Date", ylab="Product Sales")
dev.off()

##To identify the missing values and fill them using an appropriate method.
library(ggplot2)
library(imputeTS)

png('Sales Forecasting/MissingValues.png')
ggplot_na_distribution(series)
dev.off()

which(is.na(data))

series <- na_interpolation(series)
Sales_data$`Company Sales` <- na_interpolation(Sales_data$`Company Sales`)

#library(forecast)
#series <- na.interp(series, lambda = "NULL") doesnt work for me?
#print(series)

##To identify the outliers present and normalize their values.
limit <- quantile(series, 0.99)
n_series <- series
n_series[n_series>limit] <- limit

par('mar')
par(mar=c(1,1,1,1))

png('Sales Forecasting/CompanySalesPerDay_outliers.png', res=300,width = 15, height = 12, units = 'cm')
plot(series, type="l", ylab="Company Sales", xlab = "Date",
     main="Company Sales per Day")
lines(n_series, col="red")
legend("center", 
       legend = c("Original", "Trimmed"), 
       col = c("black", "red"), lty=1)
dev.off()

##To create and visualize the respective weekly and monthly series.
library(lubridate)
library(plyr)

Sales_data$week <- week(dmy(Sales_data$Date))
Sales_data$month <- month(dmy(Sales_data$Date))
Sales_data$year <- year(dmy(Sales_data$Date))

sales_ymw <- ddply(Sales_data[,c("week","month","year","Company Sales")], .(year,month,week), colwise(sum))

sales_w <- sales_ymw[,c("year","week","Company Sales")]
sales_m <- sales_ymw[,c("year","month","Company Sales")]
sales_m <- ddply(sales_m[,c("month","year","Company Sales")], .(year,month), colwise(sum))
library(zoo)
sales_m$Date <- as.yearmon(paste(sales_m$year, sales_m$month), "%Y %m")
sales_m <- sales_m[,c("Date", "Company Sales")]

png('Sales Forecasting/CompanySalesPerWeek', res=300,width = 15, height = 12, units = 'cm')
plot(sales_w$`Company Sales`, type = 'l', main = "Company Sales per week", xlab=("week"), ylab="Company Sales")
dev.off()
png('Sales Forecasting/CompanySalesPerMonth', res=300,width = 15, height = 12, units = 'cm')
plot(sales_m$`Company Sales`, type = 'l', main = "Company Sales per month", xlab=("Date"), ylab="Company Sales")
dev.off()


##To decompose the monthly series and visualize its trend and seasonal components.
sales_m <- ts(as.numeric(sales_m$`Company Sales`), frequency=12, start = c(2019,1))
dec_mult <- decompose(sales_m, type="multiplicative")
dec_add <- decompose(sales_m, type="additive")

png('Sales Forecasting/DecompositionMult', res=300,width = 15, height = 12, units = 'cm')
plot(dec_mult)
dev.off()

png('Sales Forecasting/DecompositionAdd', res=300,width = 15, height = 12, units = 'cm')
plot(dec_add)
dev.off()
##To compute the average sales per weekday and month.
aggregate( sales_ymw$`Company Sales` ~ month + year , sales_ymw , mean )
monthavg <-aggregate( sales_ymw$`Company Sales` ~ month  , sales_ymw , mean )
#write.excel(monthavg)

Sales_data$Date <- as.Date(Sales_data$Date)
Sales_data$wday <- wday(Sales_data$Date, label=TRUE)
wdayavg <- aggregate( Sales_data$`Company Sales` ~ wday , Sales_data , mean )




#Exercise B. The second series of the dataset presents the daily sales of a product sold in the store, expressed in units. You are asked:
##To visualize the series.
product_data <- Sales_data$`Product Sales`
prseries <- ts(as.numeric(product_data), frequency=365, start = c(2019,1))
plot(prseries, main = "Product Sales per day", xlab="Date", ylab="Product Sales")

##To compute the average daily demand of the product, the coefficient of variation of non-zero demands (CV2), and the average number of time periods between two successive non-zero demands (ADI).

mean(Sales_data$`Product Sales`) #ADI
(sd(Sales_data$`Product Sales`)/mean(Sales_data$`Product Sales`))^2 #CV2


ts_int <- ts(Sales_data$`Product Sales`, frequency = 365)
png('Sales Forecasting/Daily Product Sales', res=300, height = 12, width=15, units = 'cm')
plot(ts_int, ylab="Demand")
dev.off()

demand <- ts_int[ts_int!=0]
interval <- c(1) ; counter <- 1
for (i in 2:length(ts_int)) {
  if (ts_int[i]==0){
    counter <- counter + 1
  }else{
    interval <- c(interval, counter)
    counter <- 1
  }
}
stats <- data.frame(demand, interval)

mean(stats$interval) #ADI
(sd(stats$demand)/mean(stats$demand))^2 #CV2


##To visualize the empirical distribution of the demand of the product and compute its 5%, 50% and 95% percentiles.
par(mfrow=c(1,3))
boxplot(Sales_data$`Product Sales`, main="Boxplot of Product Sales")
hist(Sales_data$`Product Sales`)
plot(density(Sales_data$`Product Sales`), main="Kernel density of oilprice")

quantile(Sales_data$`Product Sales`, c(.05, .90, .95)) 

##To create and visualize the respective monthly series and comment on its seasonal pattern, if present
Sales_data<- read_csv("data/Bi-weekly assignment data.csv")

Sales_data$month <- month(dmy(Sales_data$Date))
Sales_data$year <- year(dmy(Sales_data$Date))

sales_ym <- ddply(Sales_data[,c("month","year","Product Sales")], .(year,month), colwise(sum))

sales_m <- sales_ym[,c("year","month","Product Sales")]
sales_m <- ddply(sales_m[,c("month","year","Product Sales")], .(year,month), colwise(sum))

sales_m$Date <- as.yearmon(paste(sales_m$year, sales_m$month), "%Y %m")
sales_m <- sales_m[,c("Date", "Product Sales")]

sales_m <- ts(as.numeric(sales_m$`Product Sales`), frequency=12, start = c(2019,1))
decmul <- decompose(sales_m, type="multiplicative")
plot(decmul)
decadd <- decompose(sales_m, type="additive")
plot(decadd)
