#Practical Assignment #1. This assignment is worth 150 points. 
# Due Date: Saturday, February 9th 11:55pm (EST)

#Useful hints: Utilize book examples and make sure to download 
#and library appropriate packages (FRAPO, zoo, fBasics, evir)
#Under each of the items provide the relevant R code

install.packages("xlsx")
library(xlsx)
install.packages("FRAPO")
library(FRAPO)
install.packages("fBasics")
library(fBasics)
install.packages("zoo")
library(zoo)

#1) Check you working directory
getwd()

#2) Set your working directory to RStudio folder 
#   that you have created inside the ANLY 515 forlder"
setwd('C:/HU Class/Analytics 515 51/Code')

#3) Download the followin 3 data set, and lable them 
#   BTC, LTC, ETH. These data sets reperesnet daily prices 
#   of three chryptocurrencies: Bitcoin, Litcoin, and Ethereum. 
# 1) http://mypage.siu.edu/pasha/btc.xlsx
# 2) http://mypage.siu.edu/pasha/ltc.xlsx 
# 3) http://mypage.siu.edu/pasha/eth.xlsx
#   Set the first column in each data set to the date format 
#   and the remaining columns in numerical format.
btc <- read.xlsx('btc.xlsx',1)
ltc <- read.xlsx('ltc.xlsx',1)
eth <- read.xlsx('eth.xlsx',1)

#4) Create three new datasets that are subsets of the original 
#   datasets, to include dates only after "2015-08-31". 
#   Name these datasets btcnew, ltcnew, ethnew,
btcnew <- btc[which(btc$date > "2015-08-31"),]
ltcnew <- ltc[which(ltc$date > "2015-08-31"),]
ethnew <- eth[which(eth$date > "2015-08-31"),]

#5) Create 4 variables: date (represents dates of observation) ,
#   BTCPrice (price of Bitcoin), LTCPrice (Price of Litcoin), 
#   ETHPrice (Price of Ethereum)
date <- btcnew$date
BTCPrice <- btcnew$price.USD.
LTCPrice <- ltcnew$price.USD.
ETHPrice <- ethnew$price.USD.

#6) Check the format of these variable by using str() command
str(date)
str(BTCPrice)
str(LTCPrice)
str(ETHPrice)

#7) Use date variable to create attribute "time" for 
#   BTCPrice, LTCPrice, and ETHPrice by using attr() function
attr(BTCPrice, "time") <- date
attr(LTCPrice, "time") <- date
attr(ETHPrice, "time") <- date

#8) Create three variables that represent daily returns on all three coins
#   by using returnseries()(part of FRAPO package) function. 
#   Call these variables BTCRet, LTCRet, and ETHRet. 
BTCRet <- returnseries(BTCPrice, method = "discrete",trim = TRUE)
LTCRet <- returnseries(LTCPrice, method = "discrete",trim = TRUE)
ETHRet <- returnseries(ETHPrice, method = "discrete",trim = TRUE)

#9) Use date variable to create attribute "time" for 
#   BTCRet, LTCRet, and ETHRet 
attr(BTCRet, "time") <- date[2:1239]
attr(LTCRet, "time") <- date[2:1239]
attr(ETHRet, "time") <- date[2:1239]

#10) Create a character variable CoinDates which extracts the dates 
#   from the BTCRet variable by using 
#   as.character(format(as.POSIXct(attr()),"%Y-%m-%d")) function
CoinDates <- as.character(format(as.POSIXct(attr(BTCRet,"time")),"%Y-%m-%d"))

#11) Create time series called BTCReturns by using BTCRet variable 
#   and timeSeries() function
BTCReturns <- timeSeries(BTCRet, charvec = CoinDates)

#12) Rename the column of BTCReturns to "BTCReturns" by using colnames()
colnames(BTCReturns) <- "BTCReturns"

#13) Devide the output window into 2 by 2 matrix by using par() function
par(mfrow=c(2,2))

#14) Generate a time series plot of Daily Returns of Bitcoin 
#   (requires fBasics library)
seriesPlot(BTCReturns,title=FALSE, main="Daily Returns of Bitcoin", col="blue")

#15) Generate a box plot of Returns of Bitcoin
boxPlot(BTCReturns, title=FALSE, main="Box plot of Returns of Bitcoin", col = "blue",cex = 0.5, pch = 19)

#16)  Generate a acf and pacf of Bitcoin Returns.
#     Make sure to omit missing values
acf(na.omit(BTCReturns), main="ACF of Returns of Bitcoin" , lag.max=20, ylab="", xlab= "", col="blue", ci.col="red")
pacf(na.omit(BTCReturns), main="PACF of Returns of Bitcoin", lag.max=20,ylab="", xlab = "", col = "blue", ci.col="red")

#17)  Generate a QQ plot of Bitcoin. 
#     You may have to generate a variable that omits missing values. 
#     use na.omit() function
qqnormPlot(na.omit(BTCReturns), main="QQ Plot of Returns of Bitcoin", title=FALSE, col="blue", cex=0.5, pch=19)

#18) Generate acf and pcf of the absolute returns of Bitcoin returns 
BTCReturnsAbs <- abs(BTCReturns)
acf(na.omit(BTCReturnsAbs), main="ACF of Absolute Returns of Bitcoin" , lag.max=20, ylab="", xlab= "", col="blue", ci.col="red")
pacf(na.omit(BTCReturnsAbs), main="PACF of Absolute Returns of Bitcoin", lag.max=20,ylab="", xlab = "", col = "blue", ci.col="red")

#19) Generate Volatility Clustering Plot of absolute daily returns 
# of Bitcoin 
BTCReturns100 <- tail(sort(abs(series(BTCReturns))),100) [1]
idx <- which(series(BTCReturnsAbs)>BTCReturns100, arr.ind = TRUE)
BTCReturnsAbs100 <-timeSeries(rep(0, length(BTCReturns)), charvec=time(BTCReturns))
BTCReturnsAbs100[idx,1]<- BTCReturnsAbs[idx]
plot(BTCReturnsAbs100, type="h", main="Volatility Clustering of abs daily returns of Bitcoin", ylab = "" , xlab = "", col="blue")

#Part II
#20)  Create new data set called PriceCoins that includes prices 
#     of all three coins by using cbind()
PriceCoins <- cbind(BTCPrice, LTCPrice, ETHPrice)

#21)  Check the names of the colums and the format of each of the variable
str(PriceCoins)
head(PriceCoins)

#22) by using the package zoo create an element of class "zoo" 
# labled PriceCoinsZoo, and includes Prices of all three Coints: 
# BTCPrice", "LTCPrice", and "ETHPrice"
PriceCoinsZoo <- as.zoo(PriceCoins)

#23) Plot a time series graph of prices of all three coins
plot(PriceCoinsZoo, xlab = "", main = "")

#24) Create a variable that calculates daily returns 
#   (first difference of natural logs) on different coins.
#   Call the variable ReturnCoins by using diff(log()) function
PriceCoinsRet <- diff(log(PriceCoinsZoo)) * 100

#25) Plot a time series graph of returns of all three coins
plot(PriceCoinsRet, xlab = "", main = "")

#26)  Plot cross covariance functions between returns and between 
#     absolute returns of a) BTC and LTC, BTC and ETC, and ETC and LTC. 
#     You should have 6 graphs.
layout(matrix(1:6, nrow = 3, ncol = 2, byrow = TRUE))
ccf(PriceCoinsRet[, 1], PriceCoinsRet[, 2], ylab = "", xlab = "",lag.max = 20, main = "Returns BTC vs LTC")
ccf(abs(PriceCoinsRet)[, 1], abs(PriceCoinsRet)[, 2], ylab = "", xlab = "", lag.max = 20, main = "Absolute returns BTC vs LTC")
ccf(PriceCoinsRet[, 2], PriceCoinsRet[, 3], ylab = "", xlab = "",lag.max = 20, main = "Returns LTC vs ETH")
ccf(abs(PriceCoinsRet)[, 2], abs(PriceCoinsRet)[, 3], ylab = "", xlab = "", lag.max = 20, main = "Absolute returns LTC vs ETH")
ccf(PriceCoinsRet[, 3], PriceCoinsRet[, 1], ylab = "", xlab = "",lag.max = 20, main = "Returns ETH vs BTC")
ccf(abs(PriceCoinsRet)[, 3], abs(PriceCoinsRet)[, 1], ylab = "", xlab = "", lag.max = 20, main = "Absolute returns ETH vs BTC")

#27) Generate plots of rolling correlations between 
#    BTC&LTC, BTC&ETH, and ETH&LTC
rollc <- function(x){ dim <- ncol(x)
rcor <- cor(x)[lower.tri(diag(dim), diag = FALSE)]
return(rcor)}
rcor <- rollapply(PriceCoinsRet, width = 250, rollc, align = "right", by.column = FALSE)
colnames(rcor) <- c("BTC & LTC", "LTC & ETH", "ETH & BTC")
plot(rcor, main = "", xlab = "") 

# Part III
# 28) Export all graphs to a word document and briefly analyze 
#     whether data on cryptocurrency returns resembles the 
#     "stylized facts" of the financial data

#Answer in word Doc