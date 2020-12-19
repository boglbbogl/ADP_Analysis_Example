king <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat", skip=3)
head(king, 5)
str(king)
View(king)
king_ts <- ts(king) # 시계열자료 변환
plot(king_ts, main="ts of king")
library(TTR)
par(mfrow=c(2,1))
plot(SMA(king_ts, n=3)) # 평균이 일정한지
plot(SMA(king_ts, n=10))
king_diff <- diff(king_ts, differences = 1) # 1차 차분(p,1,q)
par(mfrow=c(1,1))
plot(king_diff, main="king 1차 차분")
acf(king_diff, main="ACF of king") # MA(1)
pacf(king_diff, main="PCAF of king") # AR(3)
library(forecast)
arima(king, order = c(3,1,1)) # ARIMA(3,1,1)
auto.arima(king) # ARIMA(0,1,1)
king_arima <- arima(king, order = c(0,1,1)) # AIC가낮은 ARIMA 선정
king_fore <- forecast(king_arima) # 예측
king_fore
par(mfrow=c(1,1))
plot(king_fore)

kospi200 <- read.csv("kospiindex200.csv")
head(kospi200 ,10)
colnames(kospi200)[1] <- "day"
colnames(kospi200)[2] <- "index"
kospi <- ts(kospi200$index)
plot(kospi)
auto.arima(kospi)
kospi_arima <- arima(kospi, order = c(0,1,0))
kospi_fore <- forecast(kospi_arima)
kospi_fore
# ts() 전/후 결과같음
ko <- kospi200[-1]
ko
auto.arima(ko)